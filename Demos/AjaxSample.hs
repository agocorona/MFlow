{-# OPTIONS -XCPP #-} 

module AjaxSample ( ajaxsample) where

import Data.Monoid
import Data.ByteString.Lazy.Char8 as B
-- #define ALONE
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav ajaxsample
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

ajaxsample= do
   r <- page  $   p << b <<  "Ajax example that increment the value in a box"
            ++> do
                 let elemval= "document.getElementById('text1').value"
                 ajaxc <- ajax $ \n -> return . B.pack $ elemval <> "='" <> show(read  n +1) <>  "'"
                 b <<  "click the box "
                   ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc  elemval)] <** submitButton "submit"
   page  $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"

