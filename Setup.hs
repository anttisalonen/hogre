import System.IO
import System.Exit
import System.FilePath
import Control.Applicative
import Control.Monad

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Verbosity

main = defaultMainWithHooks $ simpleUserHooks
  {
    hookedPrograms = [simpleProgram "cgen", simpleProgram "cgen-hs"]
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

getProgram :: String -> String -> IO ConfiguredProgram
getProgram src pname = do
  mloc <- findProgramLocation normal pname
  case mloc of
    Nothing  -> err $ "Could not find program \"" ++ pname ++ "\".\n" ++ 
                      "Try installing the program from " ++ src ++ ".\n" ++ 
                      "If you've installed the program, make sure the program is in your PATH."
    Just loc -> return $ ConfiguredProgram pname Nothing [] (FoundOnSystem loc)

testBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
testBuildHook pd lb uh bf = do
  iffile   <- getSrcFile pd "if"
  igfile   <- getSrcFile pd "ig"
  hiffile  <- getSrcFile pd "hif"
  listfile <- getSrcFile pd "list"
  cgen <- getProgram "Hackage" "cgen"
  pkgconfig <- getProgram "http://pkg-config.freedesktop.org/" "pkg-config"
  headerlist <- lines <$> readFile listfile
  mogreincpath <- (filter (\w -> take 2 w == "-I") . words) <$> getProgramOutput normal pkgconfig ["--cflags", "OGRE"]
  case mogreincpath of
    []    -> err "Could not find OGRE include path with pkg-config - make sure OGRE is installed and pkg-config configured."
    (x:_) -> do
      let ogreincpath = drop 2 x
      runProgram normal cgen (["-o", "res", "--interface", iffile, "--include", ogreincpath] ++ headerlist)
      (buildHook simpleUserHooks) pd lb uh bf

