
module AutoCompList ( autocompList) where
import MFlow.Wai.Blaze.Html.All
import Data.List

autocompList= do
   r <- ask $   p <<  "Autocomplete with a list of selected entries"
            ++> p <<  "enter  and press enter"
            ++> p <<  "when su press submit, the entries are returned"
            ++> wautocompleteList "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

-- to run it alone:
--main= runNavigation "" $ transientNav autocompList
