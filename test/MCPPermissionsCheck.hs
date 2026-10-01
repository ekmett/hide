{-# LANGUAGE OverloadedStrings #-}
module MCPPermissionsCheck (checks) where

import Control.Concurrent.Async (concurrently, withAsync, poll, wait)
import Control.Exception (bracket)
import Control.Monad (foldM, unless)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Timeout (timeout)
import THC.Edit.Buffer
import THC.Edit.MCPPermissions
import THC.Edit.Model

checks :: IO ()
checks=do
  configChecks
  bracket temporary removePathForcibly $ \directory -> do
    let path=directory </> "config.toml"
        specs=[object ["name" .= name,"annotations" .= object ["readOnlyHint" .= readonly]] | (name,readonly)<-[("mutate"::T.Text,False),("read",True)]]
        core desktop _=pure (False,desktop)
        base=addDocument Nothing (newBuffer "before") (initialDesktop (80,25))
        bid=fromMaybe (error "missing initial buffer") (bufferId <$> activeWindow base)
        changed=base {buffers=M.adjust (\doc->doc {documentBuffer=replaceBuffer False "current" (documentBuffer doc)}) bid (buffers base)}
        submit runtime button desktop=case dialog desktop of
          Just dg -> let (next,effects)=submitDialog button dg desktop in snd <$> policyEffects runtime core next effects
          _ -> error "missing permission dialog"
        showSettings runtime desktop=snd <$> policyEffects runtime core desktop [PermissionAction "show" []]
        isLeft (Left _)=True
        isLeft _=False
    seen<-newIORef []
    let execute desktop name _=do
          modifyIORef' seen (++[(name,activeText desktop)])
          pure (desktop {status="executed"},pure (Right (object ["text" .= activeText desktop])))
    withPermissionsAt path specs $ \runtime -> do
      (_,readonly)<-permissionCall runtime execute base "read" (object [])
      first<-readonly
      check "read-only tools default to Enable" (not (isLeft first))
      (prompted,pending)<-permissionCall runtime execute base "mutate" (object ["revision" .= (0::Int)])
      calls<-readIORef seen
      check "mutations default to Prompt without executing" (length calls==1 && maybe False ((=="Agent permission").dialogTitle) (dialog prompted))
      executed<-withAsync pending $ \worker -> do
        blocked<-poll worker
        check "approval wait is deferred outside the initial call" (case blocked of Nothing->True; _->False)
        let current=changed {dialog=dialog prompted}
        next<-submit runtime 0 current
        result<-wait worker
        check "approval executes against CURRENT desktop and delivers deferred result"
          ((either (const Nothing) (field "text") result::Maybe T.Text)==Just "current")
        pure next
      check "approved effect returns the updated desktop" (status executed=="executed")
      (again,denied)<-permissionCall runtime execute base "mutate" (object [])
      let (escaped,effects)=handleEvent (V.EvKey V.KEsc []) again
      _<-policyEffects runtime core escaped effects
      check "Escape denies a pending request" . isLeft =<< denied
      countBefore<-length <$> readIORef seen
      check "Allow once did not enable subsequent requests" (countBefore==2)
      (cancelPrompt,cancelled)<-permissionCall runtime execute base "mutate" (object [])
      interrupted<-timeout 10000 cancelled
      check "cancelled deferred permission wait exits" (interrupted==Nothing)
      _<-submit runtime 0 cancelPrompt
      cancelledResult<-cancelled
      afterCancel<-length <$> readIORef seen
      check "cancelled request cannot execute after later approval" (isLeft cancelledResult && afterCancel==countBefore)
      settings<-showSettings runtime base
      editing<-submit runtime 0 settings
      let disabled=editing {dialog=fmap (\dg->dg {fields=[Radio "mutate" ["Enable","Prompt","Disable"] 2]}) (dialog editing)}
      _<-submit runtime 0 disabled
      (_,blocked)<-permissionCall runtime execute base "mutate" (object [])
      check "saved Disable rejects tools even with a cached registry" . isLeft =<< blocked
      persisted<-TIO.readFile path
      check "permission policy persists in its TOML namespace" ("disable" `T.isInfixOf` persisted && "permissions" `T.isInfixOf` persisted)
      (_,unknown)<-permissionCall runtime execute base "not_registered" (object [])
      check "unknown tools cannot bypass permission checks" . isLeft =<< unknown
    -- Changing the shared file is observed on every dispatch and approval.
    TIO.writeFile path "[editor.mcp.permissions]\nmutate = 'prompt'\n"
    withPermissionsAt path specs $ \runtime -> do
      before<-length <$> readIORef seen
      (prompted,pending)<-permissionCall runtime execute base "mutate" (object [])
      TIO.writeFile path "[editor.mcp.permissions]\nmutate = 'disable'\n"
      _<-submit runtime 0 prompted
      check "Disable written while queued is enforced before execution" . isLeft =<< pending
      after<-length <$> readIORef seen
      check "disabled pending callback never runs" (after==before)
      TIO.writeFile path "[broken\nsecret = 'do not leak'\n"
      (_,invalid)<-permissionCall runtime execute base "read" (object [])
      invalidResult<-invalid
      check "invalid config fails closed without exposing its contents" (case invalidResult of Left err->not ("secret" `T.isInfixOf` err); _->False)
    removeFile path
    pendingAfterClose<-withPermissionsAt path specs $ \runtime -> do
      (desktop,requests)<-foldM (\(d,results) _->do
        (next,pending)<-permissionCall runtime execute d "mutate" (object [])
        pure (next,results++[pending])) (base,[]) [1..32::Int]
      (_,overflow)<-permissionCall runtime execute desktop "mutate" (object [])
      check "permission queue is capped at 32 requests" . isLeft =<< overflow
      let other=desktop {dialog=Just (Dialog "Other editor work" Finding [] 0 ["Close"] [])}
      preserved<-tickPermissions runtime other
      check "queued approvals preserve unrelated editor dialogs" (dialog preserved==dialog other)
      pure requests
    closed<-mapM (timeout 1000000) pendingAfterClose
    check "session shutdown completes every pending waiter" (all (maybe False isLeft) closed)
  putStrLn "MCP permission checks passed"

configChecks :: IO ()
configChecks=bracket temporary removePathForcibly $ \directory -> do
  let path=directory </> "thc" </> "config.toml"
      namespace=["editor","mcp","permissions"]
      original="# Keep me\n[compiler]\noptimization = 3 # tuning\nnotes = '''literal\n[editor.mcp.permissions]\ninside string'''\n\n[editor.mcp.permissions]\nmutate = 'prompt' # keep this comment\nfuture_tool = 'disable'\n\n[editor.defaults]\nscale = 1.25 # exact\n\n[compiler.future]\nflag = true\n"
      edit values text=either (error . T.unpack) id (updateConfigTable namespace values text)
      updated=edit (object ["mutate" .= ("disable"::T.Text)]) original
  check "TOML scalar replacement preserves unrelated bytes and comments"
    (updated==T.replace "mutate = 'prompt'" "mutate = \"disable\"" original)
  let inserted=edit (object ["new_tool" .= ("enable"::T.Text)]) updated
  check "new policy keys preserve later tables and unknown tool settings"
    ("future_tool = 'disable'" `T.isInfixOf` inserted && "[compiler.future]\nflag = true\n" `T.isSuffixOf` inserted)
  let inline="editor = { mcp = { permissions = { mutate = 'prompt' } } }\n"
  check "existing inline-table scalar policies update safely"
    (edit (object ["mutate" .= ("disable"::T.Text)]) inline==T.replace "'prompt'" "\"disable\"" inline)
  check "unsafe new inline-table keys are refused without reprinting config"
    (case updateConfigTable namespace (object ["new_tool" .= ("enable"::T.Text)]) inline of Left _->True; _->False)
  absent<-readEditorDefaultsAt path
  check "absent config has empty editor defaults" (absent==Right (object []))
  saved<-writeEditorDefaultsAt path (object ["backend" .= ("terminal"::T.Text),"scale" .= (1.5::Double),"wordStar" .= True])
  check "editor defaults create global-style parent directories" (saved==Right ())
  loaded<-readEditorDefaultsAt path
  check "editor defaults roundtrip primitive TOML values" (loaded==Right (object ["backend" .= ("terminal"::T.Text),"scale" .= (1.5::Double),"wordStar" .= True]))
  TIO.writeFile path original
  _<-writeEditorDefaultsAt path (object ["scale" .= (2::Int),"blinkCursor" .= False])
  preserved<-TIO.readFile path
  check "defaults writes preserve permission and compiler namespaces" ("mutate = 'prompt' # keep this comment" `T.isInfixOf` preserved && "optimization = 3 # tuning" `T.isInfixOf` preserved)
  (a,b)<-concurrently (writeEditorDefaultsAt path (object ["columns" .= (100::Int)])) (writeEditorDefaultsAt path (object ["rows" .= (40::Int)]))
  combined<-readEditorDefaultsAt path
  check "concurrent config writers reload and retain both changes" (a==Right () && b==Right () && case combined of Right value->field "columns" value==Just (100::Int) && field "rows" value==Just (40::Int); _->False)
  let malformed="[compiler\npassword = 'hidden'\n"
  TIO.writeFile path malformed
  refused<-writeEditorDefaultsAt path (object ["scale" .= (2::Int)])
  unchanged<-TIO.readFile path
  check "invalid TOML is not overwritten or leaked" (unchanged==malformed && case refused of Left err->not ("hidden" `T.isInfixOf` err); _->False)
  bad<-writeEditorDefaultsAt path (object ["notASetting" .= True])
  check "unknown editor defaults are rejected" (case bad of Left _->True; _->False)

field :: FromJSON a => Key -> Value -> Maybe a
field key=parseMaybe (withObject "object" (.:key))

check :: String -> Bool -> IO ()
check label ok=unless ok (error label)

temporary :: IO FilePath
temporary=do
  parent<-getTemporaryDirectory
  (path,h)<-openTempFile parent "thc-permissions"
  hClose h
  removeFile path
  createDirectory path
  canonicalizePath path
