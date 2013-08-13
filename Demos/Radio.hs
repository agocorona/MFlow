
module Radio ( radio) where

import MFlow.Wai.Blaze.Html.All
import Menu

radio = do
   r <- askm $   p << b <<  "Radio buttons"
            ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

   askm $ p << ( show r ++ " selected")  ++> wlink ()  << p <<  " menu"

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav radio
