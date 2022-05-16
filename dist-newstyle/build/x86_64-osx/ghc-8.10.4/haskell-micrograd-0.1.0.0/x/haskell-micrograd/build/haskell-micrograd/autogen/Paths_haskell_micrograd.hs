{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_haskell_micrograd (
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

bindir     = "/Users/miguelangelocecedecastroneto/.cabal/bin"
libdir     = "/Users/miguelangelocecedecastroneto/.cabal/lib/x86_64-osx-ghc-8.10.4/haskell-micrograd-0.1.0.0-inplace-haskell-micrograd"
dynlibdir  = "/Users/miguelangelocecedecastroneto/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/miguelangelocecedecastroneto/.cabal/share/x86_64-osx-ghc-8.10.4/haskell-micrograd-0.1.0.0"
libexecdir = "/Users/miguelangelocecedecastroneto/.cabal/libexec/x86_64-osx-ghc-8.10.4/haskell-micrograd-0.1.0.0"
sysconfdir = "/Users/miguelangelocecedecastroneto/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_micrograd_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_micrograd_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_micrograd_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_micrograd_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_micrograd_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_micrograd_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
