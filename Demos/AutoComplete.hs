
module AutoComplete ( autocomplete1) where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.List

autocomplete1= do
   r <- askm  $   p <<  "Autocomplete "
            ++> p <<  "when su press submit, the box value  is returned"
            ++> wautocomplete Nothing filter1 <! hint "red,green or blue"
            <** submitButton "submit"
   askm  $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","red rose","green","green grass","blue","blues"]

hint s= [("placeholder",s)]

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav autocomplete1
