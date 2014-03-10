{-# OPTIONS -XCPP #-} 
module AutoCompList ( autocompList) where
import Data.List
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav autocompList
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif


autocompList= do
   r <- page  $ pageFlow "autoc" $
            p <<  "Autocomplete with a list of selected entries"
            ++> p <<  "enter  and press enter"
            ++> p <<  "when submit is pressed, the entries are returned"
            ++> wautocompleteList "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   page  $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","red rose","green","green grass","blue","blues"]

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav autocompList
