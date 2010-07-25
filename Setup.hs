import System.IO
import System.Exit
import System.Directory
import System.FilePath
import Control.Applicative
import Control.Monad
import Control.Exception(bracket)
import Data.List

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Verbosity
import Distribution.Version
import qualified Distribution.ModuleName

main = defaultMainWithHooks $ simpleUserHooks
  {
    buildHook   = ogreBuildHook
  , instHook    = ogreInstHook
  , cleanHook   = ogreCleanHook
  , haddockHook = ogreHaddockHook
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

getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext dir = 
  map (dir </>) <$> filter (\f -> takeExtension f == (extSeparator:ext)) <$> getDirectoryContents dir

resRoot :: String
resRoot = "res"

resDir :: String -> FilePath
resDir n = resRoot </> n

resCgen = resDir "cgen"
resCgenHsBase = resDir "cgen-hs"
resCgenHs = resCgenHsBase</>"Graphics"</>"Ogre"
resLib = resDir "lib"

graphFile = resDir "graph" </> "graph.txt"

inDir :: FilePath -> IO () -> IO ()
inDir path act = bracket getCwd putCwd (\_ -> setCurrentDirectory path >> act)
  where getCwd = getCurrentDirectory
        putCwd = setCurrentDirectory

pathToModuleName :: FilePath -> Distribution.ModuleName.ModuleName
pathToModuleName = Distribution.ModuleName.fromString . intercalate "." . splitBy isPathSeparator
  where splitBy :: (Char -> Bool) -> String -> [String]
        splitBy fun str = 
          let (w1, rest) = break fun str
          in if null rest
               then if null w1 then [] else [w1]
               else w1 : splitBy fun (tail rest)

fullModuleName :: String -> Distribution.ModuleName.ModuleName
fullModuleName s = Distribution.ModuleName.fromString $ "Graphics.Ogre." ++ s

mkLibrary :: IO Library
mkLibrary = do
  genhsmodulenames <- map takeBaseName <$> getFiles "hs" resCgenHs
  cppfiles         <- getFiles "cpp" resCgen
  let expModules = map fullModuleName ["HOgre", "Types"]
      libbuildinfo = BuildInfo True [] [] [] [] 
                               [(Dependency (PackageName "OgreMain")
                                           (laterVersion (Version [1,8] [])))]
                               [] cppfiles [resCgenHsBase] 
                               (filter (`notElem` expModules) 
                                           (map fullModuleName genhsmodulenames))
                               [] ["OgreMain"] [] [] [] [] [] [] [] []
                               [Dependency (PackageName "base") 
                                           (intersectVersionRanges (orLaterVersion (Version [3] [])) 
                                                                   (earlierVersion (Version [5] []))),
                                Dependency (PackageName "haskell98") anyVersion]
  return $ Library expModules True libbuildinfo
      
ogreBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
ogreBuildHook pd lb uh bf = do
  iffile   <- getSrcFile pd "if"
  igfile   <- getSrcFile pd "ig"
  hiffile  <- getSrcFile pd "hif"
  listfile <- getSrcFile pd "list"
  cgen <- getProgram "Hackage" "cgen"
  grgen <- getProgram "Hackage" "grgen"
  cgenhs <- getProgram "Hackage" "cgen-hs"
  pkgconfig <- getProgram "http://pkg-config.freedesktop.org/" "pkg-config"
  headerlist <- lines <$> readFile listfile
  mogreincpath <- (filter (\w -> take 2 w == "-I") . words) <$> getProgramOutput normal pkgconfig ["--cflags", "OGRE"]
  case mogreincpath of
    []    -> err "Could not find OGRE include path with pkg-config - make sure OGRE is installed and pkg-config configured."
    (x:_) -> do
      let ogreincpath = drop 2 x
      runProgram normal cgen (["-o", resCgen, "--interface", iffile, "--include", ogreincpath] ++ headerlist)
      runProgram normal grgen (["--interface", igfile, "--include", ogreincpath, "-o", graphFile] ++ headerlist)
      headerfiles <- getFiles "h" resCgen
      runProgram normal cgenhs (["--interface", hiffile, "-u", "HOgre.hs", "--hierarchy", "Graphics.Ogre.", "--inherit", graphFile, "-o", resCgenHs] ++ headerfiles)
      createDirectoryIfMissing True resLib
      lib <- mkLibrary
      (buildHook simpleUserHooks) pd{library = Just lib} lb uh bf

ogreInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
ogreInstHook pd lb uh ifl = do
  lib <- mkLibrary
  (instHook simpleUserHooks) pd{library = Just lib} lb uh ifl

ogreCleanHook pd n uh cf = do
    exists <- doesDirectoryExist resRoot
    when exists (removeDirectoryRecursive resRoot)
    (cleanHook simpleUserHooks) pd n uh cf

ogreHaddockHook pd lb uh hf = do
    lib <- mkLibrary
    (haddockHook simpleUserHooks) pd{library = Just lib} lb uh hf

