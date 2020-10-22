{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_personal_website (
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

bindir     = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/bin"
libdir     = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/lib/x86_64-osx-ghc-8.8.4/personal-website-0.1.0.0-HoZChkiTO6cCDWU2HPg7CJ"
dynlibdir  = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/share/x86_64-osx-ghc-8.8.4/personal-website-0.1.0.0"
libexecdir = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/libexec/x86_64-osx-ghc-8.8.4/personal-website-0.1.0.0"
sysconfdir = "/Users/wangzilin/MurakamiKennzo.github.io/.stack-work/install/x86_64-osx/54e01f566f2c78e989d635e835b1bbc076931ec8c4733da3c8cc315a847b89f3/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "personal_website_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "personal_website_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "personal_website_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "personal_website_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "personal_website_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "personal_website_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
