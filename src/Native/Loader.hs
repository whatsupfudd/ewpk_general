{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Native.Loader
  ( loadWappNative
  ) where

import Control.Exception (bracketOnError)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IORef
import Data.Text (Text, unpack)

import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.DynamicLinker as Dl

import Foreign.Ptr (FunPtr)

-- Keep handles alive so code is not unloaded while in use.
{-# NOINLINE pluginHandles #-}
pluginHandles :: IORef (Map String Dl.DL)
pluginHandles = unsafePerformIO (newIORef M.empty)

type InitPluginFun = IO ()

foreign import ccall "dynamic"
  mkInitPluginFun :: FunPtr InitPluginFun -> InitPluginFun

-- OS-specific extension:
pluginExt :: String
#if defined(darwin_HOST_OS)
pluginExt = "dylib"
#elif defined(windows_HOST_OS)
pluginExt = "dll"
#else
pluginExt = "so"
#endif


pluginFileName :: String -> FilePath
pluginFileName nativeID = "libew-wapp-" <> nativeID <> "." <> pluginExt


loadWappNative :: String -> FilePath -> String -> IO (Either String ())
loadWappNative wappID homeDir nativeID = do
  putStrLn $ "@[loadWappNative] starting, nativeID: " <> nativeID <> ", wappID: " <> wappID
  already <- M.member wappID <$> readIORef pluginHandles
  putStrLn $ "@[loadWappNative] already: " <> show already
  if already then
    pure $ Right ()  -- already loaded
  else
    let
      path = homeDir </> pluginFileName nativeID
    in do
    putStrLn $ "@[loadWappNative] path: " <> path
    inFile <- Dl.dlopen path [Dl.RTLD_NOW]
    -- If anything fails after this, close the file handle:
    bracketOnError (pure inFile) Dl.dlclose $ \fHandle -> do
      symbolAddr <- Dl.dlsym fHandle "ew_plugin_init"
      putStrLn $ "@[loadWappNative] symbols: " <> show symbolAddr
      let
        initFun = mkInitPluginFun (symbolAddr :: FunPtr InitPluginFun)
      putStrLn "@[loadWappNative] initFun: calling."
      initFun  -- this calls ewPluginInit -> registerExports
      putStrLn "@[loadWappNative] initFun: done."
      atomicModifyIORef' pluginHandles $ \aMap -> (M.insert wappID fHandle aMap, ())
      pluginHandles <- readIORef pluginHandles
      putStrLn $ "@[loadWappNative] pluginHandles: " <> show (M.keys pluginHandles)
      pure $ Right ()
