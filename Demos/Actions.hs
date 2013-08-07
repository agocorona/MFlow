
module Actions (actions) where

import MFlow.Wai.Blaze.Html.All

actions n= do
  r<- ask $   p << b <<  "Two  boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  ask $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"
  where
  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"

-- to run it alone:
--main= runNavigation "" $ transientNav actions
