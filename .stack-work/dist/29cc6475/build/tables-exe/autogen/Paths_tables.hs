{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tables (
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

bindir     = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\bin"
libdir     = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\lib\\x86_64-windows-ghc-8.8.3\\tables-0.1.0.0-IN1n313mi3h3PMbhTJv93f-tables-exe"
dynlibdir  = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\share\\x86_64-windows-ghc-8.8.3\\tables-0.1.0.0"
libexecdir = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\libexec\\x86_64-windows-ghc-8.8.3\\tables-0.1.0.0"
sysconfdir = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\tables\\.stack-work\\install\\07f464e2\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tables_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tables_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tables_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tables_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tables_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tables_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
