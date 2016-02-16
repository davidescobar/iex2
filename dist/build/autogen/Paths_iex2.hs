module Paths_iex2 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/davidescobar/projects/haskell/iex2/.cabal-sandbox/bin"
libdir     = "/Users/davidescobar/projects/haskell/iex2/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/iex2-0.1.0.0-LochqAnrKKHIXkDHIbBjbJ"
datadir    = "/Users/davidescobar/projects/haskell/iex2/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/iex2-0.1.0.0"
libexecdir = "/Users/davidescobar/projects/haskell/iex2/.cabal-sandbox/libexec"
sysconfdir = "/Users/davidescobar/projects/haskell/iex2/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "iex2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "iex2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "iex2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "iex2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "iex2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
