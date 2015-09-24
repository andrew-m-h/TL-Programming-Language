module Paths_TL (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/andrew/.cabal/bin"
libdir     = "/home/andrew/.cabal/lib/TL-0.1.0.0/ghc-7.6.3"
datadir    = "/home/andrew/.cabal/share/TL-0.1.0.0"
libexecdir = "/home/andrew/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "TL_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TL_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TL_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TL_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
