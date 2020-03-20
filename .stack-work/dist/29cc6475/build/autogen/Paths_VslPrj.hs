{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_VslPrj (
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

bindir     = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\bin"
libdir     = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\lib\\x86_64-windows-ghc-8.8.3\\VslPrj-0.1.0.0-6wa0Ba35vhnAozW5d9twL8"
dynlibdir  = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\share\\x86_64-windows-ghc-8.8.3\\VslPrj-0.1.0.0"
libexecdir = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\libexec\\x86_64-windows-ghc-8.8.3\\VslPrj-0.1.0.0"
sysconfdir = "C:\\Users\\AlekseiShestakov\\source\\Haskell\\VslPrj\\.stack-work\\install\\07f464e2\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "VslPrj_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "VslPrj_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "VslPrj_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "VslPrj_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "VslPrj_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "VslPrj_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
