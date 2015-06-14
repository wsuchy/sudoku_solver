module Paths_sudoku (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/k.suchanek/Library/Haskell/bin"
libdir     = "/Users/k.suchanek/Library/Haskell/ghc-7.8.3-x86_64/lib/sudoku-1.0"
datadir    = "/Users/k.suchanek/Library/Haskell/share/ghc-7.8.3-x86_64/sudoku-1.0"
libexecdir = "/Users/k.suchanek/Library/Haskell/libexec"
sysconfdir = "/Users/k.suchanek/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudoku_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sudoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
