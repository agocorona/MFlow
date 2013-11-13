{-# LANGUAGE OverloadedStrings, CPP #-}
module ListEdit ( wlistEd) where

import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String
-- #define ALONE
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav wlistEd
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif




wlistEd= do
   r <-  page   $   addLink
              ++> br
              ++> (wEditList El.div getString1   ["hi", "how are you"] "wEditListAdd")
              <++ br
              <** submitButton "send"

   page  $   p << (show r ++ " returned")
       ++> wlink () (p " back to menu")


   where
   addLink = a ! At.id  "wEditListAdd"
               ! href  "#"
               $ b "add"
   delBox  =  input ! type_    "checkbox"
                    ! checked  ""
                    ! onclick  "this.parentNode.parentNode.removeChild(this.parentNode)"
   getString1 mx= El.div  <<< delBox ++> getString  mx <++ br

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav wListEd

