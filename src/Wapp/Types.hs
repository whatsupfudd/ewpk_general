module Wapp.Types where

import Language.JavaScript.Inline as Js

import qualified Wapp.AppDef as Wd

data JSExecSupport = JSExecSupport {
    jsSession :: Js.Session
  , jsModule :: Js.JSVal
  , hsLibs :: Wd.NativeLibMap
  }

