module Paths_PaceCalc (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/matt/.cabal/bin"
libdir     = "/home/matt/.cabal/lib/PaceCalc-0.1/ghc-7.4.1"
datadir    = "/home/matt/.cabal/share/PaceCalc-0.1"
libexecdir = "/home/matt/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "PaceCalc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PaceCalc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PaceCalc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PaceCalc_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
