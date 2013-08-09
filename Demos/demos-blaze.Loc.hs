{-# LINE 1 "INPUT" #-}
{-# OPTIONS -XDeriveDataTypeable -XQuasiQuotes   #-}
module Main where
{-# LINE 3 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All hiding (article)
{-# LINE 4 "INPUT" #-}
import Text.Blaze.Html5 as El hiding (article)
{-# LINE 5 "INPUT" #-}
import Text.Blaze.Html5.Attributes as At hiding (step)
{-# LINE 7 "INPUT" #-}
import Data.Typeable
{-# LINE 9 "INPUT" #-}
import Data.Monoid
{-# LINE 10 "INPUT" #-}
import Data.String
{-# LINE 11 "INPUT" #-}
import Text.Hamlet
{-# LINE 14 "INPUT" #-}
import Control.Monad.Loc
{-# LINE 16 "INPUT" #-}
import TestREST
{-# LINE 17 "INPUT" #-}
import Actions
{-# LINE 18 "INPUT" #-}
import IncreaseInt
{-# LINE 19 "INPUT" #-}
import ShopCart
{-# LINE 20 "INPUT" #-}
import AjaxSample
{-# LINE 21 "INPUT" #-}
import IncreaseString
{-# LINE 22 "INPUT" #-}
import AutoCompList
{-# LINE 23 "INPUT" #-}
import ListEdit
{-# LINE 24 "INPUT" #-}
import AutoComplete
{-# LINE 25 "INPUT" #-}
import LoginSample
{-# LINE 26 "INPUT" #-}
import CheckBoxes
{-# LINE 27 "INPUT" #-}
import Multicounter
{-# LINE 28 "INPUT" #-}
import Combination
{-# LINE 29 "INPUT" #-}
import Options
{-# LINE 30 "INPUT" #-}
import ContentManagement
{-# LINE 31 "INPUT" #-}
import PreventGoingBack
{-# LINE 32 "INPUT" #-}
import Counter
{-# LINE 33 "INPUT" #-}
import PushDecrease
{-# LINE 34 "INPUT" #-}
import Dialog
{-# LINE 35 "INPUT" #-}
import PushSample
{-# LINE 36 "INPUT" #-}
import Grid
{-# LINE 37 "INPUT" #-}
import Radio
{-# LINE 38 "INPUT" #-}
import SumView
{-# LINE 42 "INPUT" #-}
main
  = Control.Monad.Loc.withLoc "main, Main(INPUT): (42, 7)"
      (do Control.Monad.Loc.withLoc "main, Main(INPUT): (43, 4)" (setAdminUser "admin" "admin")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (44, 4)" (syncWrite SyncManual)
          Control.Monad.Loc.withLoc "main, Main(INPUT): (45, 4)" (setFilesPath "Demos/")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (46, 4)" (addMessageFlows [("shop", runFlow $ shopCart `showSource` "ShopCart.hs"), ("navigation", runFlow $ transientNav testREST `showSource` "TestREST.hs")])
          Control.Monad.Loc.withLoc "main, Main(INPUT): (49, 4)" (runNavigation "" $ transientNav mainmenu))
{-# LINE 52 "INPUT" #-}
attr = fromString
{-# LINE 53 "INPUT" #-}
text = fromString
 
{-# LINE 55 "INPUT" #-}
data Options = CountI
             | CountS
             | Radio
             | Login
             | TextEdit
             | Grid
             | Autocomp
             | AutocompList
             | ListEdit
             | Shop
             | Action
             | Ajax
             | Select
             | CheckBoxes
             | PreventBack
             | Multicounter
             | Combination
             | FViewMonad
             | Counter
             | WDialog
             | Push
             | PushDec
             | Trace
             deriving (Bounded, Enum, Read, Show, Typeable)
{-# LINE 64 "INPUT" #-}
mainmenu
  = Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (64, 13)"
      (do host <- Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (65, 8)" (getRawParam "Host" `onNothing` return "mflowdemo.herokuapp.com/")
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (66, 8)" (setSessionData host)
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (67, 8)" (setHeader $ stdheader)
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (68, 8)" (setTimeouts 100 0)
          r <- Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (69, 8)"
                 (ask $
                    wcached "menu" 0 $
                      b << "PUSH" ++> (li <<< wlink Push << b << "Push example" <++ b << " A push widget in append mode receives input froma text box with autorefresh" <> article pushl) <|> (li <<< wlink PushDec << b << "A push counter" <++ b << " Show a countdown. Then goes to the main menu" <> article pushdec) <|> br ++> br ++> b << "ERROR TRACES" ++> (li <<< wlink Trace << b << " Execution traces for errors" <++ b << " produces an error and show the complete execution trace" <> article errorTrace) <|> br ++> br ++> b << "DIFFERENT KINDS OF FLOWS" ++>
                        (li $
                           Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (85, 26)"
                             (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (85, 29)" (a ! href (attr "/navigation") << " REST navigation")
                                 Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (86, 29)" (b << " Navigates trough  menus and a sucession of GET pages")
                                 Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (87, 29)" (article navigation)))
                        ++>
                        (li $
                           Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (88, 26)"
                             (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (88, 29)" (a ! href (attr "/shop") << " Stateful flow: shopping")
                                 Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (89, 29)" (b << " Add articles to a persistent shopping cart")
                                 Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (90, 29)" (article stateful)))
                        ++> br
                        ++> br
                        ++> b
                        << "BASIC"
                        ++> (li <<< wlink CountI << b << "Increase an Int" <++ b << " A loop that increases the Int value of a text box")
                        <|> (li <<< wlink CountS << b << "Increase a String" <++ b << " A loop that concatenate text in a text box")
                        <|> (li <<< wlink Select << b << "Select options" <++ b << " A combo box")
                        <|> (li <<< wlink CheckBoxes << b << "Checkboxes")
                        <|> (li <<< wlink Radio << b << "Radio buttons")
                        <++ br
                        <> br
                        <> b
                        << "WIDGET ACTIONS & CALLBACKS"
                        <|> (li <<< wlink Action << b << "Example of action, executed when a widget is validated")
                        <|> (li <<< wlink FViewMonad << b << "in page flow: sum of three numbers" <++ b << " Page flows are monadic widgets that modifies themselves in the page" <> article pageflow)
                        <|> (li <<< wlink Counter << b << "Counter" <++ b << " A page flow which increases a counter by using a callback" <> article callbacks)
                        <|> (li <<< wlink Multicounter << b << "Multicounter" <++ b << " Page flow with many independent counters with autoRefresh, so they modify themselves in-place" <> article callbacks)
                        <|> (li <<< wlink Combination << b << "Combination of three dynamic widgets" <++ b << " Combination of autoRefreshe'd widgets in the same page, withdifferent behaviours" <> article combinationl)
                        <|> (li <<< wlink WDialog << b << "Modal dialog" <++ b << " A modal Dialog box with a form within a page flow")
                        <|> br
                        ++> br
                        ++> b
                        << "DYNAMIC WIDGETS"
                        ++> (li <<< wlink Ajax << b << "AJAX example" <++ b << " A onclick event in a text box invokes a server procedure that increment the integer value" <> article ajaxl)
                        <|> (li <<< wlink Autocomp << b << "autocomplete" <++ b << " Example of autocomplete, a widget which takes the suggested values from a server procedure")
                        <|> (li <<< wlink AutocompList << b << "autocomplete List" <++ b << " Example of a widget that generates a set of return values, suggested by a autocomplete input box" <> article editList)
                        <|> (li <<< wlink ListEdit << b << "list edition" <++ b << " Example of a widget that edit, update and delete a list of user-defined widgets")
                        <|> (li <<< wlink Grid << b << "grid" <++ b << " Example of the same widget In this case, containing a row of two fields, aranged in a table" <> article gridl)
                        <|> (li <<< wlink TextEdit << b << "Content Management" <++ b << " Example of the (basic) content management primitives defined in MFlow.Forms.Widgets")
                        <|> br
                        ++> br
                        ++> b
                        << "OTHERS"
                        ++> (li <<< wlink Login << b << "login/logout" <++ b << " Example of using the login and/or logout")
                        <|> (li <<< wlink PreventBack << b << "Prevent going back after a transaction" <++ b << " Control backtracking to avoid navigating back to undo something that can not be undone. For example, a payment" <> article preventbackl))
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (149, 8)"
            (case r of
                 CountI -> clickn 0 `showSource` "IncreaseInt.hs"
                 CountS -> clicks "1" `showSource` "IncreaseString.hs"
                 Action -> actions 1 `showSource` "actions.hs"
                 Ajax -> ajaxsample `showSource` "AjaxSample.hs"
                 Select -> options `showSource` "Options.hs"
                 CheckBoxes -> checkBoxes `showSource` "CheckBoxes.hs"
                 TextEdit -> textEdit `showSource` "TextEdit.hs"
                 Grid -> grid `showSource` "Grid.hs"
                 Autocomp -> autocomplete1 `showSource` "Autocomplete.hs"
                 AutocompList -> autocompList `showSource` "AutoCompList.hs"
                 ListEdit -> wlistEd `showSource` "ListEdit.hs"
                 Radio -> radio `showSource` "Radio.hs"
                 Login -> loginSample `showSource` "LoginSample.hs"
                 PreventBack -> preventBack `showSource` "PreventGoingBack.hs"
                 Multicounter -> multicounter `showSource` "Multicounter.hs"
                 FViewMonad -> sumInView `showSource` "SumView.hs"
                 Counter -> counter `showSource` "Counter.hs"
                 Combination -> combination `showSource` "Combination.hs"
                 WDialog -> wdialog1 `showSource` "Dialog.hs"
                 Push -> pushSample `showSource` "PushSample.hs"
                 PushDec -> pushDecrease `showSource` "PushDecrease.hs"
                 Trace -> traceSample `showSource` "TraceSample.hs"))
{-# LINE 173 "INPUT" #-}
space = preEscapedToMarkup "&nbsp;"
{-# LINE 174 "INPUT" #-}
article link = space <> a ! href (attr link) << i << "(article)"
{-# LINE 176 "INPUT" #-}
pushdec = "http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html"
{-# LINE 177 "INPUT" #-}
pushl = "http://haskell-web.blogspot.com.es/2013/07/new-push-widgets-for-presentation-of.html"
{-# LINE 178 "INPUT" #-}
errorTrace = "http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html"
{-# LINE 179 "INPUT" #-}
navigation = "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html"
{-# LINE 180 "INPUT" #-}
combinationl = "http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html"
{-# LINE 181 "INPUT" #-}
pageflow = "http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html"
{-# LINE 182 "INPUT" #-}
callbacks = "http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html"
{-# LINE 183 "INPUT" #-}
gridl = "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html"
{-# LINE 184 "INPUT" #-}
editList = "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html"
{-# LINE 185 "INPUT" #-}
stateful = "http://haskell-web.blogspot.com.es/2013/04/more-on-session-management-in-mflow.html"
{-# LINE 186 "INPUT" #-}
preventbackl = "http://haskell-web.blogspot.com.es/2013/04/controlling-backtracking-in-mflow.html"
{-# LINE 187 "INPUT" #-}
ajaxl = "http://hackage.haskell.org/packages/archive/MFlow/0.3.1.0/doc/html/MFlow-Forms.html#g:17"
{-# LINE 190 "INPUT" #-}
showSource w filename
  = Control.Monad.Loc.withLoc "showSource, Main(INPUT): (190, 25)"
      (do host <- Control.Monad.Loc.withLoc "showSource, Main(INPUT): (191, 7)" (getSessionData `onNothing` error "host must have been set")
          let {-# LINE 192 "INPUT" #-}
              path = attr $ "http://" <> host <> filename
          Control.Monad.Loc.withLoc "showSource, Main(INPUT): (193, 7)" (addHeader $ source path)
          Control.Monad.Loc.withLoc "showSource, Main(INPUT): (194, 7)" (w))
  where {-# LINE 196 "INPUT" #-}
        source path html
          = El.div $
              Control.Monad.Loc.withLoc "showSource, Main(INPUT): (197, 18)"
                (do Control.Monad.Loc.withLoc "showSource, Main(INPUT): (198, 14)" (html)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (199, 14)" (br)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (200, 14)" (br)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (201, 14)" (hr)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (202, 14)" (h3 $ text "SOURCE CODE:")
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (203, 14)" (iframe ! At.style sty ! At.height (attr "400") ! At.src path $ b $ text "no iframes"))
        {-# LINE 207 "INPUT" #-}
        sty = attr "float:left;width:100%;margin-left:5px;margin-right:10px;overflow:auto;"
{-# LINE 211 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (211, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (212, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << p << "pres here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (220, 3)" (page $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          u <- Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (223, 3)" (getCurrentUser)
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (224, 3)" (page $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> p << ("user=" ++ u) ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (227, 3)" (page $ error $ "this is the error"))
{-# LINE 230 "INPUT" #-}
stdheader c = docTypeHtml $ El.head << (El.title << "MFlow examples" <> link ! rel (attr "stylesheet") ! type_ (attr "text/css") ! href (attr "http://jqueryui.com/resources/demos/style.css")) <> (body $ a ! At.style (attr "align:center;") ! href (attr "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html") << h1 << "MFlow examples" <> br <> hr <> (El.div ! At.style (attr "float:left;width:50%;margin-left:10px;margin-right:10px;overflow:auto;") $ br <> c) <> (El.div ! At.style (attr "float:right;width:45%;overflow:auto;") $ br <> p << a ! href (attr "/html/MFlow/index.html") << "MFlow package description and documentation" <> p << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") << "see demos source code" <> p << a ! href (attr "https://github.com/agocorona/MFlow/issues") << "bug tracker" <> p << a ! href (attr "https://github.com/agocorona/MFlow") << "source repository" <> p << a ! href (attr "http://hackage.haskell.org/package/MFlow") << "Hackage repository"))
  where {-# LINE 261 "INPUT" #-}
        sty = attr "float:left;width:100%;margin-left:5px;margin-right:10px;overflow:auto;"
