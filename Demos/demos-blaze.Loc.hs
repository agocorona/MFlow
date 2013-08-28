{-# LINE 1 "INPUT" #-}
{-# OPTIONS -XDeriveDataTypeable -XQuasiQuotes  #-}
module Main where
{-# LINE 3 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All hiding (name)
{-# LINE 4 "INPUT" #-}
import Text.Blaze.Html5 as El
{-# LINE 5 "INPUT" #-}
import Text.Blaze.Html5.Attributes as At hiding (step, name)
{-# LINE 6 "INPUT" #-}
import Data.TCache.IndexQuery
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
{-# LINE 40 "INPUT" #-}
import MCounter
{-# LINE 41 "INPUT" #-}
import Database
{-# LINE 43 "INPUT" #-}
import Debug.Trace
{-# LINE 45 "INPUT" #-}
(!>) = flip trace
{-# LINE 51 "INPUT" #-}
main
  = Control.Monad.Loc.withLoc "main, Main(INPUT): (51, 7)"
      (do Control.Monad.Loc.withLoc "main, Main(INPUT): (52, 4)" (setAmazonSimpleDB)
          Control.Monad.Loc.withLoc "main, Main(INPUT): (53, 4)" (index idnumber)
          Control.Monad.Loc.withLoc "main, Main(INPUT): (54, 4)" (setAdminUser "admin" "admin")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (56, 4)" (setFilesPath "Demos/")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (57, 4)"
            (runNavigation "" $
               Control.Monad.Loc.withLoc "main, Main(INPUT): (57, 23)"
                 (do Control.Monad.Loc.withLoc "main, Main(INPUT): (58, 8)" (setHeader $ stdheader)
                     Control.Monad.Loc.withLoc "main, Main(INPUT): (59, 8)" (setTimeouts 200 $ 60 * 60)
                     r <- Control.Monad.Loc.withLoc "main, Main(INPUT): (61, 8)" (step . ask $ (divmenu <<< br ++> mainMenu) <++ (El.div ! At.style (attr "float:right;width:65%;overflow:auto;") << mainmenuLinks))
                     Control.Monad.Loc.withLoc "main, Main(INPUT): (64, 8)"
                       (case r of
                            CountI -> step (clickn 0) `showSource` "IncreaseInt.hs"
                            CountS -> step (clicks "1") `showSource` "IncreaseString.hs"
                            Action -> step actions `showSource` "Actions.hs"
                            Ajax -> step ajaxsample `showSource` "AjaxSample.hs"
                            Select -> step options `showSource` "Options.hs"
                            CheckBoxes -> step checkBoxes `showSource` "CheckBoxes.hs"
                            TextEdit -> step textEdit `showSource` "TextEdit.hs"
                            Grid -> step grid `showSource` "Grid.hs"
                            Autocomp -> step autocomplete1 `showSource` "AutoComplete.hs"
                            AutocompList -> step autocompList `showSource` "AutoCompList.hs"
                            ListEdit -> step wlistEd `showSource` "ListEdit.hs"
                            Radio -> step radio `showSource` "Radio.hs"
                            Login -> step loginSample `showSource` "LoginSample.hs"
                            PreventBack -> step preventBack `showSource` "PreventGoingBack.hs"
                            Multicounter -> step multicounter `showSource` "Multicounter.hs"
                            FViewMonad -> step sumInView `showSource` "SumView.hs"
                            Counter -> step counter `showSource` "Counter.hs"
                            Combination -> step combination `showSource` "Combination.hs"
                            WDialog -> step wdialog1 `showSource` "Dialog.hs"
                            Push -> step pushSample `showSource` "PushSample.hs"
                            PushDec -> step pushDecrease `showSource` "PushDecrease.hs"
                            Trace -> step traceSample `showSource` "TraceSample.hs"
                            RESTNav -> step testREST `showSource` "TestREST.hs"
                            Database -> step database `showSource` "Database.hs"
                            ShopCart -> shopCart `showSource` "ShopCart.hs"
                            MCounter -> mcounter `showSource` "MCounter.hs"))))
{-# LINE 91 "INPUT" #-}
mainmenuLinks
  = Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (91, 16)"
      (do Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (92, 8)" (br)
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (93, 8)" (p << a ! href (attr "/html/MFlow/index.html") << "MFlow package description and documentation")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (94, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos") << "see demos source code")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (95, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow/issues") << "bug tracker")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (96, 8)" (p << a ! href (attr "https://github.com/agocorona/MFlow") << "source repository")
          Control.Monad.Loc.withLoc "mainmenuLinks, Main(INPUT): (97, 8)" (p << a ! href (attr "http://hackage.haskell.org/package/MFlow") << "Hackage repository"))
{-# LINE 103 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (103, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (104, 3)" (pagem $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << p << "pres here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (112, 3)" (pagem $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          u <- Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (115, 3)" (getCurrentUser)
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (116, 3)" (pagem $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> p << ("user=" ++ u) ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (119, 3)" (pagem $ error $ "this is the error"))
