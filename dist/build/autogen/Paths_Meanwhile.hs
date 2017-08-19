{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Meanwhile (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/leo/.cabal/bin"
libdir     = "/home/leo/.cabal/lib/x86_64-linux-ghc-8.0.1/Meanwhile-0.0.0"
dynlibdir  = "/home/leo/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/leo/.cabal/share/x86_64-linux-ghc-8.0.1/Meanwhile-0.0.0"
libexecdir = "/home/leo/.cabal/libexec"
sysconfdir = "/home/leo/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Meanwhile_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Meanwhile_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Meanwhile_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Meanwhile_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Meanwhile_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Meanwhile_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
