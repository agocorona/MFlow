

module AjaxSample ( ajaxsample) where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid
import Data.ByteString.Lazy.Char8 as B

ajaxsample= do
   r <- askm  $   p << b <<  "Ajax example that increment the value in a box"
            ++> do
                 let elemval= "document.getElementById('text1').value"
                 ajaxc <- ajax $ \n -> return . B.pack $ elemval <> "='" <> show(read  n +1) <>  "'"
                 b <<  "click the box "
                   ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc  elemval)] <** submitButton "submit"
   askm  $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"


-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav ajaxsample
