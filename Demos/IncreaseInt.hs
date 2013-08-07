
module IncreaseInt ( clickn) where

import MFlow.Wai.Blaze.Html.All

-- |+| add a widget before and after another and return both results.
-- in this case, a link wraps a form field

clickn n= do
   r <- ask $   p << b <<  "increase an Int"
            ++> wlink "menu"  << p <<  "menu"      
            |+| getInt (Just n)  <* submitButton "submit"

   case r of
    (Just _,_) -> return ()  --  ask $ wlink () << p << "thanks"
    (_, Just n') -> clickn $ n'+1

-- to run it alone:
--main= runNavigation "" $ transientNav sumWidget
