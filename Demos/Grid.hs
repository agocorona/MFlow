{-# OPTIONS  -XCPP #-}
module Grid ( grid) where

import Data.String
import Text.Blaze.Html5.Attributes as At hiding (step)

#define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

attr= fromString

grid = do
  r <- page  $ pageFlow "grid"
             $ addLink
           ++> wEditList table  row [(0,""),(0,"")] "wEditListAdd"
           <** submitButton "submit"
           
  page  $ p << (show r ++ " returned")
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

