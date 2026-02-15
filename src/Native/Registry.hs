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



data ABIResult =
    ABICompatible
  | ABIIncompatible Word32

type NativeFunMap = HashMap Text Wd.NativeLibFunction
type ExportDefs = [(Text, [(Text, Wd.NativeLibFunction)])]


{-# NOINLINE globalNativeMap #-}
globalNativeMap :: IORef NativeFunMap
globalNativeMap = unsafePerformIO (newIORef HM.empty)


registerExports :: ExportDefs -> IO ()
registerExports exportDefs = do
  putStrLn $ "@[registerExports] exportDefs: " <> show (map fst exportDefs)
  atomicModifyIORef' globalNativeMap $ \aMap ->
    (foldl (\accumA (groupName, fctDefs) ->
        foldl (\accumB (fctName, fctPtr) -> HM.insert (groupName <> "." <> fctName) fctPtr accumB) accumA fctDefs
      ) aMap exportDefs, ()
    )


showDefs :: ExportDefs -> String
showDefs exportDefs =
  unlines $ map (\(groupName, items) -> "Group: " <> show groupName <> "\n" <> unlines (map (\(fctName, _) -> "  " <> show fctName) items)) exportDefs


lookupNative :: Text -> IO (Maybe Wd.NativeLibFunction)
lookupNative name = do
  hMap <- readIORef globalNativeMap
  putStrLn $ "@[lookupNative] hMap: " <> show (HM.keys hMap)
  pure $ HM.lookup name hMap


registerExportsWithABI :: Word32 -> ExportDefs -> IO ABIResult
registerExportsWithABI abi exportDefs =
  if abi == ABI.nativeABI then 
      registerExports exportDefs >> pure ABICompatible
  else
    pure $ ABIIncompatible abi
