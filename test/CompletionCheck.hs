module CompletionCheck (checks) where
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List (sort)
import System.Console.GetOpt
import System.Directory
import System.FilePath ((</>), addTrailingPathSeparator)
import System.IO (hClose, openTempFile)
import THC.Edit.Completion

checks :: IO ()
checks = bracket temporary removePathForcibly $ \directory -> do
  let check name good=unless good (error name)
      descriptors=[Option ['h'] ["help"] (NoArg ()) "",
        Option [] ["appearance"] (ReqArg (const ()) "light|dark|system") "",
        Option [] ["mode"] (ReqArg (const ()) "NUMBER") "",
        Option [] ["ssh"] (ReqArg (const ()) "HOST") "",
        Option [] ["never-run"] (NoArg (error "completion evaluated an option")) ""]
      complete=bashCompletion descriptors
      absolute name=directory </> name
      prefix=addTrailingPathSeparator directory
  options <- complete ["1","thc-edit","--he"]
  check "completion filters option prefixes from descriptors" (options==["--help"])
  shorts <- complete ["1","thc-edit","-h"]
  check "completion includes short option aliases" (shorts==["-h"])
  mapM_ (\args -> complete args >>= check "invalid completion indices are quiet" . null)
    [[],["nope","thc-edit",""],["-1","thc-edit",""],["0","thc-edit"],["2","thc-edit",""],["1"]]
  choices <- complete ["2","thc-edit","--appearance","d"]
  inline <- complete ["1","thc-edit","--appearance=d"]
  modes <- complete ["2","thc-edit","--mode",""]
  check "completion handles separate and inline option choices" (choices==["dark"] && inline==["--appearance=dark"])
  check "completion offers implemented screen modes" (all (`elem` modes) ["3","259"])
  writeFile (absolute "file with spaces.hs") "unchanged"
  createDirectory (absolute "folder with spaces")
  paths <- complete ["1","thc-edit",prefix++"f"]
  check "completion preserves spaces and marks directories" (sort paths==sort [absolute "file with spaces.hs",addTrailingPathSeparator (absolute "folder with spaces")])
  afterValue <- complete ["3","thc-edit","--ssh","example.test",prefix++"file"]
  check "completion consumes option values before positional paths" (afterValue==[absolute "file with spaces.hs"])
  unknownValue <- complete ["2","thc-edit","--ssh",prefix]
  check "completion does not treat hosts as local paths" (null unknownValue)
  writeFile (absolute "--helpful.hs") ""
  literal <- withCurrentDirectory directory (complete ["2","thc-edit","--","--he"])
  check "completion honors the option terminator" (literal==["--helpful.hs"])
  empty <- withCurrentDirectory directory (complete ["1","thc-edit",""])
  check "empty words complete current directory without losing relative spelling"
    (all (`elem` empty) ["--help","file with spaces.hs",addTrailingPathSeparator "folder with spaces"])
  let hostile="$(touch should-not-exist).hs"
  writeFile (absolute hostile) ""
  before <- sort <$> listDirectory directory
  raw <- withCurrentDirectory directory (complete ["1","thc-edit","$("])
  after <- sort <$> listDirectory directory
  untouched <- readFile (absolute "file with spaces.hs")
  noEvaluation <- complete ["1","thc-edit","--never"]
  check "completion neither evaluates words/options nor modifies files"
    (raw==[hostile] && before==after && untouched=="unchanged" && noEvaluation==["--never-run"])
  missing <- complete ["1","thc-edit",absolute "missing"++"/file"]
  check "unreadable or missing completion directories are quiet" (null missing)
  putStrLn "bash completion checks passed"
  where
    temporary = do
      base<-getTemporaryDirectory
      (path,handle)<-openTempFile base "thc-completion-"
      hClose handle
      removeFile path
      createDirectory path
      pure path
