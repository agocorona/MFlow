{-# OPTIONS  -XDeriveDataTypeable -XQuasiQuotes  #-}
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
import Control.Workflow
(!>)= flip trace

main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath "Demos/"
   runNavigation ""  mainmenu


attr= fromString


mainmenu ::FlowM Html (Workflow IO) ()
mainmenu =  do
       setHeader $ stdheader 
       setTimeouts 300 0
       let trans= transientNav
       r <- transientNav . ask $ (El.div ! At.style (attr "background-color:#EEEEEE;float:left\
                         \;width:30%\
                         \;margin-left:10px;margin-right:10px;overflow:auto;")
                       <<< br ++>  mainMenu) 
                 <++ (El.div ! At.style (attr "float:right;width:65%;overflow:auto;") << mainmenuLinks)

       case r of
             CountI    ->  trans $ clickn 0              `showSource`  "IncreaseInt.hs"
             CountS    ->  trans $ clicks "1"            `showSource`  "IncreaseString.hs"
             Action    ->  trans $ actions               `showSource`  "actions.hs"
             Ajax      ->  trans $ ajaxsample            `showSource`  "AjaxSample.hs"
             Select    ->  trans $ options               `showSource`  "Options.hs"
             CheckBoxes -> trans $ checkBoxes            `showSource`  "CheckBoxes.hs"
             TextEdit  ->  trans $ textEdit              `showSource`  "TextEdit.hs"
             Grid      ->  trans $ grid                  `showSource`  "Grid.hs"
             Autocomp  ->  trans $ autocomplete1         `showSource`  "Autocomplete.hs"
             AutocompList -> trans $ autocompList        `showSource`  "AutoCompList.hs"
             ListEdit  ->  trans $ wlistEd               `showSource`  "ListEdit.hs"
             Radio     ->  trans $ radio                 `showSource`  "Radio.hs"
             Login     ->  trans $ loginSample           `showSource`  "LoginSample.hs"
             PreventBack -> trans $ preventBack          `showSource`  "PreventGoingBack.hs"
             Multicounter-> trans $ multicounter         `showSource`  "Multicounter.hs"
             FViewMonad  -> trans $ sumInView            `showSource`  "SumView.hs"
             Counter     -> trans $ counter              `showSource`  "Counter.hs"
             Combination -> trans $ combination          `showSource`  "Combination.hs"
             WDialog     -> trans $ wdialog1             `showSource`  "Dialog.hs"
             Push        -> trans $ pushSample           `showSource`  "PushSample.hs"
             PushDec     -> trans $ pushDecrease         `showSource`  "PushDecrease.hs"
             Trace       -> trans $ traceSample          `showSource`  "TraceSample.hs"
             RESTNav     -> trans $ testREST             `showSource`  "TestREST.hs"
             ShopCart    -> shopCart                     `showSource`  "ShopCart.hs"


mainmenuLinks= do  -- using the blaze-html monad
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



