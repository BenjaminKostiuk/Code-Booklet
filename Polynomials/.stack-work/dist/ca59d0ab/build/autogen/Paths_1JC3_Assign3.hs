{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_1JC3_Assign3 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\bin"
libdir     = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\lib\\x86_64-windows-ghc-8.0.2\\1JC3-Assign3-0.1.0.0-EuZjAisOsWwAS3BuVKIX1H"
dynlibdir  = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\share\\x86_64-windows-ghc-8.0.2\\1JC3-Assign3-0.1.0.0"
libexecdir = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\libexec"
sysconfdir = "C:\\Users\\benko\\GitRepos\\benjaminkostiuk\\Project3_Polynomials\\.stack-work\\install\\22a81968\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "1JC3_Assign3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "1JC3_Assign3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "1JC3_Assign3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "1JC3_Assign3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
