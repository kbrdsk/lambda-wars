module Paths_lambda_wars (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\s\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\s\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\lambda-wars-0.1.0.0"
datadir    = "C:\\Users\\s\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\lambda-wars-0.1.0.0"
libexecdir = "C:\\Users\\s\\AppData\\Roaming\\cabal\\lambda-wars-0.1.0.0"
sysconfdir = "C:\\Users\\s\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambda_wars_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambda_wars_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lambda_wars_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambda_wars_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambda_wars_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
