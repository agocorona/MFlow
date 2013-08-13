
module CheckBoxes ( checkBoxes) where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid

checkBoxes= do
   r <- askm  $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                           <> (setCheckBox False "Green" <++ b <<  "green")
                           <> (setCheckBox False "blue"  <++ b <<  "blue"))
              <** submitButton "submit"


   askm  $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")


-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav checkBoxes
