{-# OPTIONS  -XCPP #-}
module Radio ( radio) where

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

radio = do
   r <- page $   p << b <<  "Radio buttons"
            ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

   page $ p << ( show r ++ " selected")  ++> wlink ()  << p <<  " menu"

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav radio
