
module IncreaseInt ( clickn) where

import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu

-- |+| add a widget before and after another and return both results.
-- in this case, a link wraps a form field

clickn :: Int -> FlowM Html IO ()
clickn n= do
   r <- page  $ p << b <<  "increase an Int"
            ++> wlink "menu"  << p <<  "menu"      
            |+| getInt (Just n)  <* submitButton "submit"

   case r of
    (Just _,_) -> return ()  --  page  $ wlink () << p << "thanks"
    (_, Just n') -> clickn $ n'+1

-- to run it alone, change page by page  and uncomment this:
--main= runNavigation "" $ transientNav sumWidget
