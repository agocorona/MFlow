
module Actions (actions) where

import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu

actions = do
  r<- page  $   p << b <<  "Two  boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  page  $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"
  where
  action n=  page  $ getString (Just $ n ++ " action")<** submitButton "submit action"

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav actions
