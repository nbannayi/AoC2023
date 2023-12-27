{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_LensLibrary (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/bin"
libdir     = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/lib/x86_64-osx-ghc-9.6.3/LensLibrary-0.1.0.0-9F6cXKmjCLI1FRomSbkwCE-LensLibrary"
dynlibdir  = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/lib/x86_64-osx-ghc-9.6.3"
datadir    = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/share/x86_64-osx-ghc-9.6.3/LensLibrary-0.1.0.0"
libexecdir = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/libexec/x86_64-osx-ghc-9.6.3/LensLibrary-0.1.0.0"
sysconfdir = "/Users/Nick/development/workspace/AoC2023/Day15/.stack-work/install/x86_64-osx/b0b1a6ec3d8f1e54a4089d69a285dec54d7b29cf3d233368ee005975a500ea78/9.6.3/etc"

getBinDir     = catchIO (getEnv "LensLibrary_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "LensLibrary_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "LensLibrary_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "LensLibrary_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LensLibrary_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LensLibrary_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
