module Paths_MFlow (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,5], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5\\ghc-7.0.3"
datadir    = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5"
libexecdir = "C:\\Users\\agocorona\\AppData\\Roaming\\cabal\\MFlow-0.0.5"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "MFlow_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "MFlow_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "MFlow_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "MFlow_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
