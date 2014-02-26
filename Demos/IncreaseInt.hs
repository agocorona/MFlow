{-# OPTIONS -XCPP #-}
module IncreaseInt ( clickn) where

#define ALONE
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All 
main= runNavigation "" . transientNav $ clickn 0
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

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

