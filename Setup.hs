import System.IO

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

main = defaultMainWithHooks $ simpleUserHooks
  {
    buildHook = testBuildHook
  }

testBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
testBuildHook pd lb uh bf = (buildHook simpleUserHooks) pd lb uh bf

