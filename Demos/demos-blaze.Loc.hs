{-# LINE 1 "INPUT" #-}
{-# OPTIONS -XDeriveDataTypeable -XQuasiQuotes    #-}
module Main where
{-# LINE 3 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All
{-# LINE 4 "INPUT" #-}
import Text.Blaze.Html5 as El
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
      (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (65, 8)" (setHeader stdheader)
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (66, 8)" (setTimeouts 100 0)
          r <- Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (67, 8)" (ask $ wcached "menu" 0 $ b << "PUSH" ++> br ++> wlink Push << b << "Example of a widget with push" <|> br ++> wlink PushDec << b << "A push counter" <|> br ++> br ++> b << "ERROR TRACES" ++> br ++> wlink Trace << b << "Execution traces for errors" <|> br ++> br ++> b << "DIFFERENT KINDS OF FLOWS" ++> br ++> a ! href (attr "/navigation") << "REST navigation" ++> br ++> a ! href (attr "/shop") << "stateful flow: shopping" ++> br ++> br ++> b << "BASIC" ++> br ++> wlink CountI << b << "increase an Int" <|> br ++> wlink CountS << b << "increase a String" <|> br ++> wlink Select << b << "select options" <|> br ++> wlink CheckBoxes << b << "checkboxes" <|> br ++> wlink Radio << b << "Radio buttons" <++ br <> br <> b << "WIDGET ACTIONS & CALLBACKS" <|> br ++> wlink Action << b << "Example of action, executed when a widget is validated" <|> br ++> wlink FViewMonad << b << "in page flow: sum of three numbers" <|> br ++> wlink Counter << b << "Counter" <|> br ++> wlink Multicounter << b << "Multicounter" <|> br ++> wlink Combination << b << "combination of three active widgets" <|> br ++> wlink WDialog << b << "modal dialog" <|> br ++> br ++> b << "DYNAMIC WIDGETS" ++> br ++> wlink Ajax << b << "AJAX example" <|> br ++> wlink Autocomp << b << "autocomplete" <|> br ++> wlink AutocompList << b << "autocomplete List" <|> br ++> wlink ListEdit << b << "list edition" <|> br ++> wlink Grid << b << "grid" <|> br ++> wlink TextEdit << b << "Content Management" <|> br ++> br ++> b << "OTHERS" ++> br ++> wlink Login << b << "login/logout" <|> br ++> wlink PreventBack << b << "Prevent going back after a transaction")
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (108, 8)"
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
{-# LINE 137 "INPUT" #-}
showSource w filename
  = Control.Monad.Loc.withLoc "showSource, Main(INPUT): (137, 25)"
      (do host <- Control.Monad.Loc.withLoc "showSource, Main(INPUT): (138, 7)" (getRawParam "Host" `onNothing` return "localhost")
          let {-# LINE 139 "INPUT" #-}
              path = attr $ "http://" <> host <> ('/' : filename)
          Control.Monad.Loc.withLoc "showSource, Main(INPUT): (140, 7)" (addHeader $ source path)
          Control.Monad.Loc.withLoc "showSource, Main(INPUT): (141, 7)" (w))
  where {-# LINE 143 "INPUT" #-}
        source path html
          = El.div $
              Control.Monad.Loc.withLoc "showSource, Main(INPUT): (144, 18)"
                (do Control.Monad.Loc.withLoc "showSource, Main(INPUT): (145, 14)" (html)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (146, 14)" (br)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (147, 14)" (br)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (148, 14)" (hr)
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (149, 14)" (h3 $ text "SOURCE CODE:")
                    Control.Monad.Loc.withLoc "showSource, Main(INPUT): (150, 14)" (iframe ! At.style sty ! At.height (attr "400") ! At.src path $ b $ text "no iframes"))
        {-# LINE 154 "INPUT" #-}
        sty = attr "float:left;width:100%;margin-left:5px;margin-right:10px;overflow:auto;"
{-# LINE 158 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (158, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (159, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << p << "pres here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (167, 3)" (page $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          u <- Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (170, 3)" (getCurrentUser)
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (171, 3)" (page $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> p << ("user=" ++ u) ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (174, 3)" (page $ error $ "this is the error"))
{-# LINE 177 "INPUT" #-}
stdheader c = docTypeHtml $ El.head << (El.title << "MFlow examples" <> link ! rel (attr "stylesheet") ! type_ (attr "text/css") ! href (attr "http://jqueryui.com/resources/demos/style.css")) <> (body $ a ! At.style (attr "align:center;") ! href (attr "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html") << h1 << "MFlow examples" <> br <> hr <> (El.div ! At.style (attr "float:left;width:50%;margin-left:10px;margin-right:10px;overflow:auto;") $ br <> c) <> (El.div ! At.style (attr "float:right;width:45%;overflow:auto;") $ br <> p << a ! href (attr "/html/MFlow/index.html") << "MFlow package description and documentation" <> p << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") << "see demos source code" <> p << a ! href (attr "https://github.com/agocorona/MFlow/issues") << "bug tracker" <> p << a ! href (attr "https://github.com/agocorona/MFlow") << "source repository" <> p << a ! href (attr "http://hackage.haskell.org/package/MFlow") << "Hackage repository" <> [shamlet| <script type="text/javascript" src="http://output18.rssinclude.com/output?type=js&amp;id=727700&amp;hash=8aa6c224101cac4ca2a7bebd6e28a2d7"></script>|]))
