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



main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath "Demos/"
   addMessageFlows  [("shop"      , runFlow $ shopCart  `showSource` "ShopCart.hs")
                    ,("navigation", runFlow $ transientNav testREST `showSource` "TestREST.hs")]

   runNavigation "" $  transientNav mainmenu


attr= fromString
text= fromString

data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes | PreventBack | Multicounter
            | Combination
            | FViewMonad | Counter | WDialog |Push |PushDec |Trace
            deriving (Bounded, Enum,Read, Show,Typeable)


mainmenu=   do
       setHeader stdheader 
       setTimeouts 100 0
       r <- ask $  wcached "menu" 0 $
               b <<  "PUSH"
               ++> br ++> wlink Push    << b << "Example of a widget with push"
               <|> br ++> wlink PushDec << b << "A push counter"
               <|> br ++> br
                 ++> b <<  "ERROR TRACES"
               ++> br ++> wlink Trace << b << "Execution traces for errors"
               <|> br ++> br ++>
               b <<  "DIFFERENT KINDS OF FLOWS"
               ++> br ++>  a ! href (attr "/navigation") <<  "REST navigation"   -- ordinary Blaze.Html link
               ++> br ++>  a ! href (attr "/shop") <<  "stateful flow: shopping"   -- ordinary Blaze.Html link
                       
               ++>  br ++> br ++> b <<  "BASIC"
               ++>  br ++> wlink CountI       << b <<  "increase an Int"
               <|>  br ++> wlink CountS       << b <<  "increase a String"
               <|>  br ++> wlink Select       << b <<  "select options"
               <|>  br ++> wlink CheckBoxes   << b <<  "checkboxes"
               <|>  br ++> wlink Radio        << b <<  "Radio buttons"

               <++  br <>  br                 <> b <<  "WIDGET ACTIONS & CALLBACKS"
               <|>  br ++> wlink Action       << b <<  "Example of action, executed when a widget is validated"
               <|>  br ++> wlink FViewMonad   << b <<  "in page flow: sum of three numbers"
               <|>  br ++> wlink Counter      << b <<  "Counter"
               <|>  br ++> wlink Multicounter << b <<  "Multicounter"
               <|>  br ++> wlink Combination  << b <<  "combination of three active widgets"
               <|>  br ++> wlink WDialog      << b <<  "modal dialog"

               <|>  br ++>  br               ++> b <<  "DYNAMIC WIDGETS"
               ++>  br ++> wlink Ajax         << b <<  "AJAX example"
               <|>  br ++> wlink Autocomp     << b <<  "autocomplete"
               <|>  br ++> wlink AutocompList << b <<  "autocomplete List"
               <|>  br ++> wlink ListEdit     << b <<  "list edition"
               <|>  br ++> wlink Grid         << b <<  "grid"
               <|>  br ++> wlink TextEdit     << b <<  "Content Management"

               <|>  br ++>  br ++> b <<  "OTHERS"
               ++>  br ++> wlink Login        << b <<  "login/logout"
               <|>  br ++> wlink PreventBack  << b <<  "Prevent going back after a transaction"



       case r of
             CountI    ->  clickn 0               `showSource`  "IncreaseInt.hs"
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


   



showSource w filename = do
      host <- getRawParam "Host" `onNothing` return "http://mflowdemo.herokuapp.com/"
      let path=  attr $ "http://" <> host <> ('/':filename)
      addHeader $ source path
      w
      where
      source path html =
        El.div $ do
             html
             br
             br
             hr
             h3 $ text "SOURCE CODE:"
             iframe ! At.style sty
                    ! At.height (attr "400")
                    ! At.src path
                    $ b $ text "no iframes"
      sty= attr "float:left\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"

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


stdheader c= docTypeHtml
   $ El.head << (El.title << "MFlow examples"
     <> link ! rel   ( attr "stylesheet")
             ! type_ ( attr "text/css")
             ! href (attr "http://jqueryui.com/resources/demos/style.css"))
   <> (body $ 
      a ! At.style (attr "align:center;")
        ! href ( attr  "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html")
        << h1 <<  "MFlow examples"
   <> br
   <> hr
   <> (El.div ! At.style (attr "float:left\
                         \;width:50%\
                         \;margin-left:10px;margin-right:10px;overflow:auto;") $
          br <> c)
   <> (El.div ! At.style (attr "float:right;width:45%;overflow:auto;") $

       br
       <> p  << a ! href (attr "/html/MFlow/index.html") <<  "MFlow package description and documentation"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") <<  "see demos source code"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/issues") <<  "bug tracker"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow") <<  "source repository"
       <> p  << a ! href (attr "http://hackage.haskell.org/package/MFlow") <<  "Hackage repository"
       <> [shamlet| <script type="text/javascript" src="http://output18.rssinclude.com/output?type=js&amp;id=727700&amp;hash=8aa6c224101cac4ca2a7bebd6e28a2d7"></script>|]

              ))
