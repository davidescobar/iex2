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

bindir     = "/home/dev/projects/haskell/iex2/.stack-work/install/i386-linux/lts-5.3/7.10.3/bin"
libdir     = "/home/dev/projects/haskell/iex2/.stack-work/install/i386-linux/lts-5.3/7.10.3/lib/i386-linux-ghc-7.10.3/iex2-0.1.0.0-2nNxlz5jI3K5xjzqmQ86IX"
datadir    = "/home/dev/projects/haskell/iex2/.stack-work/install/i386-linux/lts-5.3/7.10.3/share/i386-linux-ghc-7.10.3/iex2-0.1.0.0"
libexecdir = "/home/dev/projects/haskell/iex2/.stack-work/install/i386-linux/lts-5.3/7.10.3/libexec"
sysconfdir = "/home/dev/projects/haskell/iex2/.stack-work/install/i386-linux/lts-5.3/7.10.3/etc"

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
