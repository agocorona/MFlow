
module IncreaseInt ( clickn) where

import MFlow.Wai.Blaze.Html.All
import Menu

-- |+| add a widget before and after another and return both results.
-- in this case, a link wraps a form field

clickn n= do
   r <- askm  $ p << b <<  "increase an Int"
            ++> wlink "menu"  << p <<  "menu"      
            |+| getInt (Just n)  <* submitButton "submit"

   case r of
    (Just _,_) -> return ()  --  askm  $ wlink () << p << "thanks"
    (_, Just n') -> clickn $ n'+1

-- to run it alone, change askm by askm  and uncomment this:
--main= runNavigation "" $ transientNav sumWidget
