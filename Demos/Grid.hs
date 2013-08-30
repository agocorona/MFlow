
module Grid ( grid) where

import MFlow.Wai.Blaze.Html.All
import Menu
import Data.String
import Text.Blaze.Html5.Attributes as At hiding (step)

attr= fromString

grid = do
  r <- askm  $   addLink
           ++> wEditList table  row ["",""] "wEditListAdd"
           <** submitButton "submit"
           
  askm  $   p << (show r ++ " returned")
      ++> wlink () (p <<  " back to menu")
      
  where
  row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 0)
                          <*> tdborder <<< getTextBox (Just "")
                          <++ tdborder << delLink)
                          
  addLink= a ! href (attr "#")
             ! At.id (attr "wEditListAdd")
             <<  "add"
             
  delLink= a ! href (attr "#")
             ! onclick (attr "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode)")
             <<  "delete"
             
  tdborder= td ! At.style  (attr "border: solid 1px")

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav grid
