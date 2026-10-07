{-# LANGUAGE OverloadedStrings #-}
module LinksCheck (checks) where
import SourceWindowFixture (sourceFixtureBuffer)
import Control.Exception (bracket)
import Control.Monad (unless,forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Directory
import System.FilePath
import System.IO
import Hide.Buffer
import Hide.Conversation (renderReply)
import Hide.GuestAccess
import Hide.Links
import Hide.Markdown
import Hide.Model
import Hide.Syntax (linkSpans,styledContents)

checks :: IO ()
checks=bracket temporary removePathForcibly $ \root->do
  createDirectory (root </> "docs")
  writeFile (root </> "README.md") "# Project\n\nRead [Installation](docs/install.md).\n"
  writeFile (root </> "docs/install.md") "# Installation\n\n[Home](../README.md)\n\n## Linux fonts\n\nInstall fonts.\n"
  let d=(initialDesktop (80,25)) {defaultDirectory=Just root}
      check label ok=unless ok (error label)
      field key=parseMaybe (withObject "packet" (.: key))
      doc state=maybe (error "missing document") id (activeDocument state)
      text=contents.documentBuffer.doc
  (help,packet)<-followLink True d (Just (root </> "README.md")) ""
  check "Markdown opens inside editor" (packet==Nothing && "Installation" `T.isInfixOf` text help && not ("docs/install.md" `T.isInfixOf` text help))
  check "help retains clickable target and source base" (documentMarkdownPath (doc help)==Just (root </> "README.md") && map (\(_,_,u)->u) (documentLinks (doc help))==["docs/install.md"])
  (installation,_)<-followLink True help (documentMarkdownPath (doc help)) "docs/install.md#linux-fonts"
  check "relative doc and anchor navigation" (documentMarkdownPath (doc installation)==Just (root </> "docs/install.md") && maybe False ((>0).scrollRow) (activeWindow installation))
  (home,_)<-followLink True installation (documentMarkdownPath (doc installation)) "../README.md"
  check "nested link resolves against current document" (documentMarkdownPath (doc home)==Just (root </> "README.md"))
  let a=case documentLinks (doc help) of (start,_,_):_->start; []->error "missing link"
      (row,col)=bufferLineColumn (documentBuffer (doc help)) a
      w=maybe (error "window") id (activeWindow help)
      x=left (bounds w)+1+col; y=top (bounds w)+1+row
      (pressed,startEffects)=handleEvent (V.EvMouseDown x y V.BLeft []) help
      (_,clickEffects)=handleEvent (V.EvMouseUp x y (Just V.BLeft)) pressed
      (dragged,_)=handleEvent (V.EvMouseDown (x+2) y V.BLeft []) pressed
      (_,dragEffects)=handleEvent (V.EvMouseUp (x+2) y (Just V.BLeft)) dragged
      (menu,_)=handleEvent (V.EvMouseDown x y V.BRight []) help
  check "click activates on release, drag remains selection" (null startEffects && clickEffects==[FollowLink (SourceLink (Just (root </> "README.md"))) "docs/install.md"] && null dragEffects)
  check "context menu offers linked target" (case contextKind menu of LinkContext (OpenLink _ "docs/install.md")->True; _->False)
  let chat=help {buffers=M.adjust (\value->value {documentLabel=Just "Conversation"}) (sourceFixtureBuffer w) (buffers help)}
      hiddenAt=top (composerRect chat w)
      hidden=chat {buffers=M.adjust (\value->value {documentBuffer=newBuffer (T.replicate 50 "link\n"),documentLinks=[(0,249,"https://example.com/")]}) (sourceFixtureBuffer w) (buffers chat)}
  check "composer cannot follow transcript links hidden beneath it" (linkAt x hiddenAt hidden==Nothing)
  forM_ [12,40,100] $ \width->forM_ [False,True] $ \outgoing->do
    let styled=renderReply True width outgoing "Some [wide 界 label](https://example.com/path) text."
        spans=linkSpans styled
        labels=T.concat [T.take (b-a') (T.drop a' (styledContents styled)) | (a',b,_)<-spans]
    check "chat links survive wrapping and right/left alignment" (not (null spans) && all (\(_,_,url)->url=="https://example.com/path") spans && "界" `T.isInfixOf` labels && not ("http" `T.isInfixOf` labels))
  (_,url)<-followLink True d Nothing "https://example.com/a?b=1&c=2"
  check "URL packet goes to frontend" ((url >>= field "url") == Just ("https://example.com/a?b=1&c=2"::T.Text))
  BS.writeFile (root </> "picture.png") (BS.pack [137,80,78,71])
  (_,picture)<-followLink True d (Just (root </> "picture.png")) ""
  check "image packet carries bounded bytes, not server path" ((picture >>= field "mime") == Just ("image/png"::T.Text) && (picture >>= field "data") == Just ("iVBORw=="::T.Text))
  forM_ ["javascript:alert(1)","data:text/html,test","https://", "https://example.com/\n"] $ \urlText->do
    check "unsafe URL rejected" (not (validWebURL urlText))
    result<-openResource (object ["url" .= (urlText::T.Text)])
    check "client rejects unsafe open without invoking OS" (either (const True) (const False) result)
  check "agent input cannot launch browser outside tool permissions" (not (guestCommandAllowed (OpenLink (SourceLink Nothing) "https://example.com")) && not (guestEffectsAllowed [FollowLink (SourceLink Nothing) "https://example.com"]))
  putStrLn "Links checks passed"
  where
    temporary=do
      base<-getTemporaryDirectory
      (path,h)<-openTempFile base "thc-links-check"
      hClose h; removeFile path; createDirectory path
      canonicalizePath path
