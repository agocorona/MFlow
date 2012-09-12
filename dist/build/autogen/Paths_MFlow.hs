module Paths_MFlow (
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
version = Version {versionBranch = [0,0,5,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5.2\\ghc-7.4.1"
datadir    = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5.2"
libexecdir = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5.2"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "MFlow_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MFlow_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MFlow_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MFlow_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
