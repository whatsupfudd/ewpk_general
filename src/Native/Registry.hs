module Native.Registry
  ( NativeFunMap
  , ABIResult(..)
  , registerExports
  , lookupNative
  , registerExportsWithABI
  ) where

import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Word (Word32)

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Aeson as Ae

import qualified Native.ABI as ABI
import qualified Wapp.AppDef as Wd

-- Whatever your actual signature is:
type NativeFunMap = HashMap Text Wd.NativeLibFunction

{-# NOINLINE globalNativeMap #-}
globalNativeMap :: IORef NativeFunMap
globalNativeMap = unsafePerformIO (newIORef HM.empty)


registerExports :: [(Text, Wd.NativeLibFunction)] -> IO ()
registerExports exports = do
  putStrLn $ "@[registerExports] exports: " <> show (map fst exports)
  atomicModifyIORef' globalNativeMap $ \m ->
    (foldl (\acc (k, v) -> HM.insert k v acc) m exports, ())


lookupNative :: Text -> IO (Maybe Wd.NativeLibFunction)
lookupNative name = do
  hMap <- readIORef globalNativeMap
  putStrLn $ "@[lookupNative] hMap: " <> show (HM.keys hMap)
  pure $ HM.lookup name hMap


data ABIResult = ABICompatible | ABIIncompatible Word32


registerExportsWithABI :: Word32 -> [(Text, Wd.NativeLibFunction)] -> IO ABIResult
registerExportsWithABI abi xs =
  if abi == ABI.nativeABI
    then registerExports xs >> pure ABICompatible
    else pure (ABIIncompatible abi)
