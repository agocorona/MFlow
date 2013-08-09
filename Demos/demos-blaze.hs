{-# OPTIONS  -XDeriveDataTypeable -XQuasiQuotes  -F -pgmF MonadLoc #-}
module Main where
import MFlow.Wai.Blaze.Html.All hiding (article)
import Text.Blaze.Html5 as El hiding (article)
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
       host <- getRawParam "Host" `onNothing` return "mflowdemo.herokuapp.com/"
       setSessionData host
       setHeader $ stdheader 
       setTimeouts 100 0
       r <- ask $  wcached "menu" 0 $
               b <<  "PUSH"
               ++> (li <<< wlink Push << b << "Push example"
                                 <++ b << " A push widget in append mode receives input from\
                                         \a text box with autorefresh"
                                 <>  article pushl)
               <|> (li <<< wlink PushDec << b << "A push counter"
                                 <++ b << " Show a countdown. Then goes to the main menu"
                                 <>  article pushdec)
               <|> br ++> br
               ++> b <<  "ERROR TRACES"
               ++> (li <<< wlink Trace << b << " Execution traces for errors"
                             <++ b << " produces an error and show the complete execution trace"
                             <>  article errorTrace)
               <|> br ++> br ++>
               b <<  "DIFFERENT KINDS OF FLOWS"
               ++> (li $ do a ! href (attr "/navigation") <<  " REST navigation"
                            b << " Navigates trough  menus and a sucession of GET pages"
                            article navigation)
               ++> (li $ do a ! href (attr "/shop") <<  " Stateful flow: shopping"
                            b << " Add articles to a persistent shopping cart"
                            article stateful)
                       
               ++>  br ++> br ++> b <<  "BASIC"
               ++>  (li <<< wlink CountI       << b <<  "Increase an Int"
                                   <++ b << " A loop that increases the Int value of a text box")  
               <|>  (li <<< wlink CountS       << b <<  "Increase a String"
                                   <++ b << " A loop that concatenate text in a text box")  
               <|>  (li <<< wlink Select       << b <<  "Select options"
                                   <++ b << " A combo box")
               <|>  (li <<< wlink CheckBoxes   << b <<  "Checkboxes")
           
               <|>  (li <<< wlink Radio        << b <<  "Radio buttons")

               <++  br <>  br                 <> b <<  "WIDGET ACTIONS & CALLBACKS"
               <|>  (li <<< wlink Action       << b <<  "Example of action, executed when a widget is validated")
               <|>  (li <<< wlink FViewMonad   << b <<  "in page flow: sum of three numbers"
                             <++ b << " Page flows are monadic widgets that modifies themselves in the page"
                             <>  article pageflow)
               <|>  (li <<< wlink Counter      << b <<  "Counter"
                             <++ b << " A page flow which increases a counter by using a callback"
                             <>  article callbacks)
               <|>  (li <<< wlink Multicounter << b <<  "Multicounter"
                             <++ b << " Page flow with many independent counters with autoRefresh, so they modify themselves in-place"
                             <>  article callbacks)
               <|>  (li <<< wlink Combination  << b <<  "Combination of three dynamic widgets"
                             <++ b << " Combination of autoRefreshe'd widgets in the same page, with\
                                      \different behaviours"
                             <>  article combinationl)
               <|>  (li <<< wlink WDialog      << b <<  "Modal dialog"
                             <++ b << " A modal Dialog box with a form within a page flow")          

               <|>  br ++>  br               ++> b <<  "DYNAMIC WIDGETS"
               ++>  (li <<< wlink Ajax         << b <<  "AJAX example"
                             <++ b << " A onclick event in a text box invokes a server procedure that \
                                      \increment the integer value"
                             <>  article ajaxl)
               <|>  (li <<< wlink Autocomp     << b <<  "autocomplete"
                             <++ b << " Example of autocomplete, a widget which takes the suggested values from a server procedure")                 
               <|>  (li <<< wlink AutocompList << b <<  "autocomplete List"
                             <++ b << " Example of a widget that generates a set of return values, suggested by a autocomplete input box"
                             <>  article editList)
               <|>  (li <<< wlink ListEdit     << b <<  "list edition"
                             <++ b << " Example of a widget that edit, update and delete a list of user-defined widgets")
               <|>  (li <<< wlink Grid         << b <<  "grid"
                             <++ b << " Example of the same widget In this case, containing a row of two fields, aranged in a table"
                             <>  article gridl)
               <|>  (li <<< wlink TextEdit     << b <<  "Content Management"
                             <++ b << " Example of the (basic) content management primitives defined in MFlow.Forms.Widgets")

               <|>  br ++>  br ++> b <<  "OTHERS"
               ++>  (li <<< wlink Login        << b <<  "login/logout"
                             <++ b << " Example of using the login and/or logout")
               <|>  (li <<< wlink PreventBack  << b <<  "Prevent going back after a transaction"
                             <++ b << " Control backtracking to avoid navigating back to undo something that can not be undone\
                                      \. For example, a payment"
                             <>  article preventbackl)



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

space= preEscapedToMarkup "&nbsp;"
article link=  space <> a ! href (attr link) <<  i << "(article)"

pushdec= "http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html"
pushl= "http://haskell-web.blogspot.com.es/2013/07/new-push-widgets-for-presentation-of.html"
errorTrace= "http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html"
navigation= "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html"
combinationl= "http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html"
pageflow= "http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html"
callbacks= "http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html"
gridl= "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html"
editList= "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html"
stateful= "http://haskell-web.blogspot.com.es/2013/04/more-on-session-management-in-mflow.html"
preventbackl= "http://haskell-web.blogspot.com.es/2013/04/controlling-backtracking-in-mflow.html"
ajaxl= "http://hackage.haskell.org/packages/archive/MFlow/0.3.1.0/doc/html/MFlow-Forms.html#g:17"


showSource w filename = do
      host <- getSessionData `onNothing` error "host must have been set"
      let path=  attr $ "http://" <> host <> filename
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


stdheader  c= docTypeHtml
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
--       <> (h3 $ do
--             text "SOURCE CODE:"
--             iframe ! At.style sty
--                    ! At.height (attr "400")
--                    ! At.src (attr $   "http://" <> host <> ('/':"demos-blaze.hs"))
--                    $ b $ text "no iframes")
                ))
   where
   sty= attr "float:left\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"
            
