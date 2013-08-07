
module CheckBoxes ( checkBoxes) where
import MFlow.Wai.Blaze.Html.All
import Data.Monoid

checkBoxes= do
   r <- ask $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                           <> (setCheckBox False "Green" <++ b <<  "green")
                           <> (setCheckBox False "blue"  <++ b <<  "blue"))
              <** submitButton "submit"


   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")


-- to run it alone:
--main= runNavigation "" $ transientNav checkBoxes
