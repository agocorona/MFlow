{-# OPTIONS  -XCPP #-}
module Dialog (wdialog1) where

-- #define ALONE -- to execute it alone, uncomment this

#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav wdialog1
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

wdialog1= do
   page   wdialogw
   page  (wlink () << "out of the page flow, press here to go to the menu")

wdialogw= pageFlow "diag" $ do
   r <- p << "please enter your name" ++> getString (Just "your name") <** submitButton "ok"
   wdialog "({modal: true})" "Question"  $ 
           p << ("Do your name is \""++r++"\"?") ++> getBool True "yes" "no" <** submitButton "ok"

  `wcallback` \q -> if not q
                      then wdialogw
                      else  wlink () << b << "thanks, press here to exit from the page Flow"

