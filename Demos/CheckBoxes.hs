{-# OPTIONS -XCPP #-} 
module CheckBoxes ( checkBoxes) where

import Data.Monoid
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav autocomplete1
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

checkBoxes= do
   r <- page  $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                           <> (setCheckBox False "Green" <++ b <<  "green")
                           <> (setCheckBox False "blue"  <++ b <<  "blue"))
              <** submitButton "submit"


   page  $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

