
module Actions (actions) where

import MFlow.Wai.Blaze.Html.All
import Menu

actions n= do
  r<- askm  $   p << b <<  "Two  boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  askm  $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"
  where
  action n=  askm  $ getString (Just $ n ++ " action")<** submitButton "submit action"

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav actions
