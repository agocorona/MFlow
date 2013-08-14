{-# LINE 1 "INPUT" #-}
{-# OPTIONS -XDeriveDataTypeable -XQuasiQuotes   #-}
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
import Menu
{-# LINE 17 "INPUT" #-}
import TestREST
{-# LINE 18 "INPUT" #-}
import Actions
{-# LINE 19 "INPUT" #-}
import IncreaseInt
{-# LINE 20 "INPUT" #-}
import ShopCart
{-# LINE 21 "INPUT" #-}
import AjaxSample
{-# LINE 22 "INPUT" #-}
import IncreaseString
{-# LINE 23 "INPUT" #-}
import AutoCompList
{-# LINE 24 "INPUT" #-}
import ListEdit
{-# LINE 25 "INPUT" #-}
import AutoComplete
{-# LINE 26 "INPUT" #-}
import LoginSample
{-# LINE 27 "INPUT" #-}
import CheckBoxes
{-# LINE 28 "INPUT" #-}
import Multicounter
{-# LINE 29 "INPUT" #-}
import Combination
{-# LINE 30 "INPUT" #-}
import Options
{-# LINE 31 "INPUT" #-}
import ContentManagement
{-# LINE 32 "INPUT" #-}
import PreventGoingBack
{-# LINE 33 "INPUT" #-}
import Counter
{-# LINE 34 "INPUT" #-}
import PushDecrease
{-# LINE 35 "INPUT" #-}
import Dialog
{-# LINE 36 "INPUT" #-}
import PushSample
{-# LINE 37 "INPUT" #-}
import Grid
{-# LINE 38 "INPUT" #-}
import Radio
{-# LINE 39 "INPUT" #-}
import SumView
{-# LINE 41 "INPUT" #-}
import Debug.Trace
{-# LINE 42 "INPUT" #-}
import Control.Workflow
{-# LINE 43 "INPUT" #-}
(!>) = flip trace
{-# LINE 45 "INPUT" #-}
main
  = Control.Monad.Loc.withLoc "main, Main(INPUT): (45, 7)"
      (do Control.Monad.Loc.withLoc "main, Main(INPUT): (46, 4)" (setAdminUser "admin" "admin")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (47, 4)" (syncWrite SyncManual)
          Control.Monad.Loc.withLoc "main, Main(INPUT): (48, 4)" (setFilesPath "Demos/")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (49, 4)" (runNavigation "" mainmenu))
{-# LINE 52 "INPUT" #-}
attr = fromString
 
{-# LINE 55 "INPUT" #-}
newtype Server = Server String
               deriving Typeable
 
{-# LINE 56 "INPUT" #-}
mainmenu :: FlowM Html (Workflow IO) ()
{-# LINE 57 "INPUT" #-}
mainmenu
  = Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (57, 13)"
      (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (58, 8)" (setHeader $ stdheader)
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (59, 8)" (setTimeouts 300 0)
          let {-# LINE 60 "INPUT" #-}
              trans = transientNav
          r <- Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (61, 8)" (transientNav . ask $ (El.div ! At.style (attr "background-color:#EEEEEE;float:left;width:30%;margin-left:10px;margin-right:10px;overflow:auto;") <<< br ++> mainMenu) <++ (El.div ! At.style (attr "float:right;width:65%;overflow:auto;") << mainmenuLinks))
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (67, 8)"
            (case r of
                 CountI -> trans $ clickn 0 `showSource` "IncreaseInt.hs"
                 CountS -> trans $ clicks "1" `showSource` "IncreaseString.hs"
                 Action -> trans $ actions `showSource` "actions.hs"
                 Ajax -> trans $ ajaxsample `showSource` "AjaxSample.hs"
                 Select -> trans $ options `showSource` "Options.hs"
                 CheckBoxes -> trans $ checkBoxes `showSource` "CheckBoxes.hs"
                 TextEdit -> trans $ textEdit `showSource` "TextEdit.hs"
                 Grid -> trans $ grid `showSource` "Grid.hs"
                 Autocomp -> trans $ autocomplete1 `showSource` "Autocomplete.hs"
                 AutocompList -> trans $ autocompList `showSource` "AutoCompList.hs"
                 ListEdit -> trans $ wlistEd `showSource` "ListEdit.hs"
                 Radio -> trans $ radio `showSource` "Radio.hs"
                 Login -> trans $ loginSample `showSource` "LoginSample.hs"
                 PreventBack -> trans $ preventBack `showSource` "PreventGoingBack.hs"
                 Multicounter -> trans $ multicounter `showSource` "Multicounter.hs"
                 FViewMonad -> trans $ sumInView `showSource` "SumView.hs"
                 Counter -> trans $ counter `showSource` "Counter.hs"
                 Combination -> trans $ combination `showSource` "Combination.hs"
                 WDialog -> trans $ wdialog1 `showSource` "Dialog.hs"
                 Push -> trans $ pushSample `showSource` "PushSample.hs"
                 PushDec -> trans $ pushDecrease `showSource` "PushDecrease.hs"
                 Trace -> trans $ traceSample `showSource` "TraceSample.hs"
                 RESTNav -> trans $ testREST `showSource` "TestREST.hs"
                 ShopCart -> shopCart `showSource` "ShopCart.hs"))
{-# LINE 94 "INPUT" #-}
mainmenuLinks
  = Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (94, 16)"
      (do Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (95, 8)" (br)
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (96, 8)" (p << a ! href (attr "/html/MFlow/index.html") << "MFlow package description and documentation")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (97, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") << "see demos source code")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (98, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow/issues") << "bug tracker")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (99, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow") << "source repository")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (100, 8)" (p << a ! href (attr "http://hackage.haskell.org/package/MFlow") << "Hackage repository"))
{-# LINE 104 "INPUT" #-}
showSource w filename
  = Control.Monad.Loc.withLoc "showSource, Main(INPUT): (104, 25)"
      (do Control.Monad.Loc.withLoc "showSource, Main(INPUT): (105, 7)" (setSessionData filename)
          Control.Monad.Loc.withLoc "showSource, Main(INPUT): (106, 7)" (w))
{-# LINE 110 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (110, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (111, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << p << "pres here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (119, 3)" (page $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          u <- Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (122, 3)" (getCurrentUser)
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (123, 3)" (page $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> p << ("user=" ++ u) ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (126, 3)" (page $ error $ "this is the error"))
