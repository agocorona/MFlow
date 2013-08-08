
module Radio ( radio) where

import MFlow.Wai.Blaze.Html.All

radio = do
   r <- ask $   p << b <<  "Radio buttons"
            ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

   ask $ p << ( show r ++ " selected")  ++> wlink ()  << p <<  " menu"

-- to run it alone:
--main= runNavigation "" $ transientNav radio
