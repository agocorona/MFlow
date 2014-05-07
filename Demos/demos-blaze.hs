{-# OPTIONS  -XDeriveDataTypeable -XQuasiQuotes -XOverloadedStrings #-}
module Main where
import MFlow.Wai.Blaze.Html.All hiding (name)
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step,name)
import Data.TCache.IndexQuery
import Data.Typeable


import Data.Monoid
import Data.String
import Text.Hamlet

-- For error traces
import Control.Monad.Loc

import Menu   hiding (page)
import TestREST
import Actions
import IncreaseInt
import ShopCart
import AjaxSample
import IncreaseString
import AutoCompList
import ListEdit
import AutoComplete
import LoginSample
import CheckBoxes
import Multicounter
import Combination
import Options
import ContentManagement
import PreventGoingBack
import Counter
import PushDecrease
import Dialog
import PushSample
import Grid
import Radio
import SumView
import MCounter
import Database
import MFlowPersistent
import RuntimeTemplates
import TraceSample
import AcidState
import SearchCart
import InitialConfig
import GenerateForm
import GenerateFormUndo
import GenerateFormUndoMsg
import WebService
import CachingDataset
import LazyLoad
import Data.TCache.DefaultPersistence


import Data.ByteString.Lazy.Char8 hiding (index)
instance Serializable Int where
  serialize= pack . show
  deserialize= read . unpack



main= do
   index tfieldKey
   setAdminUser adminname  adminname
   userRegister edadmin edadmin
   userRegister "edituser" "edituser"
   syncWrite  $ Asyncronous 120 defaultCheck  1000

   setFilesPath "Demos/"
   addMessageFlows[
       -- Web Services --
       ("apirest", wstateless restService),
       ("apikv"  , wstateless keyValueService),
       ("apiparser", wstateless  parserService)]
   runNavigation "" $ do

       setHeader $ stdheader 
       setTimeouts 400 $ 60 * 60
       r <- step . page $ do
                       us <- getCurrentUser
                       if us == anonymous then public else private
                       tFieldEd edadmin "head" "set Header" <++ hr
                       **> (El.div ! At.style "float:right" <<<   wlogin)
                       **> (divmenu  <<< br ++>  mainMenu) 
                       <** (El.div ! At.style "float:right;width:65%;overflow:auto;"
                            <<< (pageFlow "del"  (tFieldEd edadmin "intro" "enter intro text"))
                            <++ do
                                hr
                                disquscript
                                )
 

       case r of
             Wiki      ->    delSessionData (Filename "") >> transientNav  wiki                 
             CountI    ->     transientNav  (clickn 0)           `showSource`  "IncreaseInt.hs"
             CountS    ->     transientNav  (clicks "1")         `showSource`  "IncreaseString.hs"
             Action    ->     transientNav  actions              `showSource`  "Actions.hs"
             Ajax      ->     transientNav  ajaxsample           `showSource`  "AjaxSample.hs"
             Select    ->     transientNav  options              `showSource`  "Options.hs"
             CheckBoxes ->    transientNav  checkBoxes           `showSource`  "CheckBoxes.hs"
             TextEdit  ->     transientNav  textEdit             `showSource`  "ContentManagement.hs"
             Grid      ->     transientNav  grid                 `showSource`  "Grid.hs"
             Autocomp  ->     transientNav  autocomplete1        `showSource`  "AutoComplete.hs"
             AutocompList ->  transientNav  autocompList         `showSource`  "AutoCompList.hs"
             ListEdit  ->     transientNav  wlistEd              `showSource`  "ListEdit.hs"
             Radio     ->     transientNav  radio                `showSource`  "Radio.hs"
             Login     ->     transientNav  loginSample          `showSource`  "LoginSample.hs"
             PreventBack ->   transientNav  preventBack          `showSource`  "PreventGoingBack.hs"
             Multicounter->   transientNav  multicounter         `showSource`  "Multicounter.hs"
             FViewMonad  ->   transientNav  sumInView            `showSource`  "SumView.hs"
             Counter     ->   transientNav  counter              `showSource`  "Counter.hs"
             Combination ->   transientNav  combination          `showSource`  "Combination.hs"
             WDialog     ->   transientNav  wdialog1             `showSource`  "Dialog.hs"
             Push        ->   transientNav  pushSample           `showSource`  "PushSample.hs"
             PushDec     ->   transientNav  pushDecrease         `showSource`  "PushDecrease.hs"
             Trace       ->   transientNav  traceSample          `showSource`  "TraceSample.hs"
             RESTNav     ->   transientNav  testREST             `showSource`  "TestREST.hs"
             Database    ->   transientNav  database             `showSource`  "Database.hs"
             ShopCart    ->   shopCart                           `showSource`  "ShopCart.hs"
             MCounter    ->   mcounter                           `showSource`  "MCounter.hs"
             MFlowPersist ->  transientNav mFlowPersistent       `showSource`  "MFlowPersistent.hs"
             RuntimeTemplates -> transientNav runtimeTemplates   `showSource`  "RuntimeTemplates.hs"
             AcidState        -> transientNav (acidState undefined)     `showSource`  "AcidState.hs"
             InitialConfig -> initialConfig                      `showSource`  "InitialConfig.hs"
             SearchCart    -> searchCart                         `showSource`  "SearchCart.hs"
             GenerateForm  -> transientNav genForm
             GenerateFormUndo -> transientNav genFormUndo
             GenerateFormUndoMsg -> transientNav genFormUndoMsg
             CacheDataset -> transientNav cachingDataset         `showSource` "CachingDataset.hs"
             LazyLoad     -> transientNav lazyLoad               `showSource` "LazyLoad.hs"

