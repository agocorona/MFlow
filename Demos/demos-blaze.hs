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

import Menu hiding (page)
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


import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8 hiding (index)
instance Serializable Int where
  serialize= pack . show
  deserialize= read . unpack



main= do
--   index idnumber                      -- for the Database example
   index tfieldKey
   setAdminUser adminname adminname
   userRegister edadmin edadmin
   userRegister "edituser" "edituser"
   syncWrite  $ Asyncronous 120 defaultCheck  1000
   db <- initAcid -- for the AcidState example
   setFilesPath "Demos/"
   runNavigation "" $ do
       setHeader $ stdheader 
       setTimeouts 400 $ 60 * 60

       r <- step . page $   tFieldEd edadmin "head" "set Header" <++ hr
                       **> (El.div ! At.style "float:right" <<< pageFlow "login" (autoRefresh wlogin))
                       **> (divmenu  <<< br ++>  mainMenu) 
                       <** (El.div ! At.style "float:right;width:65%;overflow:auto;"
                            <<< tFieldEd edadmin "intro" "enter intro text")

       case r of
             Wiki      ->    delSessionData (Filename "") >> step  wiki                 
             CountI    ->     step  (clickn 0)           `showSource`  "IncreaseInt.hs"
             CountS    ->     step  (clicks "1")         `showSource`  "IncreaseString.hs"
             Action    ->     step  actions              `showSource`  "Actions.hs"
             Ajax      ->     step  ajaxsample           `showSource`  "AjaxSample.hs"
             Select    ->     step  options              `showSource`  "Options.hs"
             CheckBoxes ->    step  checkBoxes           `showSource`  "CheckBoxes.hs"
             TextEdit  ->     step  textEdit             `showSource`  "ContentManagement.hs"
             Grid      ->     step  grid                 `showSource`  "Grid.hs"
             Autocomp  ->     step  autocomplete1        `showSource`  "AutoComplete.hs"
             AutocompList ->  step  autocompList         `showSource`  "AutoCompList.hs"
             ListEdit  ->     step  wlistEd              `showSource`  "ListEdit.hs"
             Radio     ->     step  radio                `showSource`  "Radio.hs"
             Login     ->     step  loginSample          `showSource`  "LoginSample.hs"
             PreventBack ->   step  preventBack          `showSource`  "PreventGoingBack.hs"
             Multicounter->   step  multicounter         `showSource`  "Multicounter.hs"
             FViewMonad  ->   step  sumInView            `showSource`  "SumView.hs"
             Counter     ->   step  counter              `showSource`  "Counter.hs"
             Combination ->   step  combination          `showSource`  "Combination.hs"
             WDialog     ->   step  wdialog1             `showSource`  "Dialog.hs"
             Push        ->   step  pushSample           `showSource`  "PushSample.hs"
             PushDec     ->   step  pushDecrease         `showSource`  "PushDecrease.hs"
             Trace       ->   step  traceSample          `showSource`  "TraceSample.hs"
             RESTNav     ->   step  testREST             `showSource`  "TestREST.hs"
             Database    ->   step  database              `showSource`  "Database.hs"
             ShopCart    ->   shopCart                   `showSource` "ShopCart.hs"
             MCounter    ->   mcounter                   `showSource` "MCounter.hs"
             MFlowPersist ->  step mFlowPersistent       `showSource` "MFlowPersistent.hs"
             RuntimeTemplates -> step runtimeTemplates   `showSource` "RuntimeTemplates.hs"
             AcidState        -> step (acidState db)     `showSource` "AcidState.hs"
             InitialConfig -> initialConfig              `showSource` "InitialConfig.hs"
             SearchCart    -> searchCart                `showSource` "SearchCart.hs"



