{-# OPTIONS -XCPP #-} 
module AutoComplete ( autocomplete1) where
import Data.List
-- #define ALONE
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav autocomplete1
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

autocomplete1= do
   r <- page  $   p <<  "Autocomplete "
            ++> p <<  "when submit is pressed, the box value  is returned"
            ++> wautocomplete Nothing filter1 <! hint "red,green or blue"
            <** submitButton "submit"
   page  $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","red rose","green","green grass","blue","blues"]

hint s= [("placeholder",s)]

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav autocomplete1
