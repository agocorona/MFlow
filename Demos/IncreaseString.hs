
module IncreaseString ( clicks) where

import MFlow.Wai.Blaze.Html.All
import Menu

clicks s= do
   s' <- askm  $  p << b <<  "increase a String"
             ++> p << b <<  "press the back button to go back to the menu"
             ++>(getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just (b << "length must be < 5") else Nothing )
   clicks $ s'++ "1"


-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav clicks "1"
