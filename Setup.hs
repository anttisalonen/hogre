import System.IO
import System.Exit
import System.FilePath

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

main = defaultMainWithHooks $ simpleUserHooks
  {
    hookedPrograms = [simpleProgram "cgen-hs"]
  , buildHook = testBuildHook
  }

err :: String -> IO a
err str = hPutStrLn stderr str >> exitWith (ExitFailure 1)

getSrcFile :: PackageDescription -> String -> IO FilePath
getSrcFile pd ext = do
  let files = filter (\f -> takeExtension f == (extSeparator:ext)) $ extraSrcFiles pd
  case files of
    [file] -> return file
    files  -> err $ "Error in extra source files configuration: Exactly one file with the extension \"" ++ ext ++ "\" allowed. Found:\n\t" ++ show files

testBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
testBuildHook pd lb uh bf = do
  iffile   <- getSrcFile pd "if"
  igfile   <- getSrcFile pd "ig"
  hiffile  <- getSrcFile pd "hif"
  listfile <- getSrcFile pd "list"
  
  (buildHook simpleUserHooks) pd lb uh bf

