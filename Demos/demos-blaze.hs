{-# OPTIONS  -XDeriveDataTypeable -XQuasiQuotes  -F -pgmF MonadLoc #-}
module Main where
import MFlow.Wai.Blaze.Html.All 
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)

import Data.Typeable

import Data.Monoid
import Data.String
import Text.Hamlet

-- For error traces
import Control.Monad.Loc

import Menu
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

import Debug.Trace
(!>)= flip trace

main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath "Demos/"
   addMessageFlows  [("shop"      , runFlow $ shopCart  `showSource` "ShopCart.hs")
                    ,("navigation", runFlow $ transientNav testREST `showSource` "TestREST.hs")]

   runNavigation "" $  transientNav mainmenu


attr= fromString


newtype Server= Server String deriving Typeable
mainmenu =  do
       setHeader $ stdheader 
       setTimeouts 100 0
       
       r <- ask $ (El.div ! At.style (attr "background-color:#EEEEEE;float:left\
                         \;width:30%\
                         \;margin-left:10px;margin-right:10px;overflow:auto;")
                       <<< br ++>  mainMenu) 
                 <++ (El.div ! At.style (attr "float:right;width:65%;overflow:auto;") << mainmenuLinks)



       case r of
             CountI    ->  clickn 0              `showSource`  "IncreaseInt.hs"
             CountS    ->  clicks "1"            `showSource`  "IncreaseString.hs"
             Action    ->  actions 1             `showSource`  "actions.hs"
             Ajax      ->  ajaxsample            `showSource`  "AjaxSample.hs"
             Select    ->  options               `showSource`  "Options.hs"
             CheckBoxes -> checkBoxes            `showSource`  "CheckBoxes.hs"
             TextEdit  ->  textEdit              `showSource`  "TextEdit.hs"
             Grid      ->  grid                  `showSource`  "Grid.hs"
             Autocomp  ->  autocomplete1         `showSource`  "Autocomplete.hs"
             AutocompList -> autocompList        `showSource`  "AutoCompList.hs"
             ListEdit  ->  wlistEd               `showSource`  "ListEdit.hs"
             Radio     ->  radio                 `showSource`  "Radio.hs"
             Login     ->  loginSample           `showSource`  "LoginSample.hs"
             PreventBack -> preventBack          `showSource`  "PreventGoingBack.hs"
             Multicounter-> multicounter         `showSource`  "Multicounter.hs"
             FViewMonad  -> sumInView            `showSource`  "SumView.hs"
             Counter    -> counter               `showSource`  "Counter.hs"
             Combination -> combination          `showSource`  "Combination.hs"
             WDialog     -> wdialog1             `showSource`  "Dialog.hs"
             Push        -> pushSample           `showSource`  "PushSample.hs"
             PushDec     -> pushDecrease         `showSource`  "PushDecrease.hs"
             Trace       -> traceSample          `showSource`  "TraceSample.hs"

mainmenuLinks= do
       br
       p  << a ! href (attr "/html/MFlow/index.html") <<  "MFlow package description and documentation"
       p  << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") <<  "see demos source code"
       p  << a ! href (attr "https://github.com/agocorona/MFlow/issues") <<  "bug tracker"
       p  << a ! href (attr "https://github.com/agocorona/MFlow") <<  "source repository"
       p  << a ! href (attr "http://hackage.haskell.org/package/MFlow") <<  "Hackage repository"



showSource w filename = do
      setSessionData filename
      w

     

traceSample= do
  page $   h2 << "Error trace example"
       ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism"
       ++> p << "It is more detailed than a call stack"
       ++> p << "this example has a deliberate error"
       ++> br
       ++> p << "You must be logged as admin to see the trace"
       ++> wlink () << p << "pres here"

  page $   p <<  "Please login with admin/admin"
       ++> userWidget (Just "admin") userLogin

  u <- getCurrentUser
  page $   p << "The trace will appear after you press the link. press one of the options available at the bottom of the page"
       ++> p << ("user="++ u) ++> br
       ++> wlink () << "press here"
  page $   error $ "this is the error"



