{-# OPTIONS -XCPP #-} 
module ContentManagement ( textEdit) where

import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.TCache.Memoization

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav testEdit
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

-- to run it alone, change askm by ask and uncomment this:
--askm = ask
--main= runNavigation "" $ transientNav textEdit



editUser= "edituser"

textEdit= do
    userRegister editUser editUser
    let first=  p << i <<
                   (El.span <<  "this is a page with"
                   <> b <<  " two " <> El.span <<  "paragraphs. this is the first")

        second= p << i <<  "This is the original  of the second paragraph"

    page  $   p << b <<  "An example of content management"
        ++> first
        ++> second
        ++> wlink ()  << p <<  "click here to edit it"
    
    page  $   p <<  "Please login with edituser/edituser to edit it"
        ++> userWidget (Just editUser) userLogin
     
    page  $   p <<  "Now you can click the fields and edit them"
        ++> p << b <<  "to save an edited field, press the save icon in the left"
        ++> tFieldEd editUser "first"  first
        **> tFieldEd editUser "second" second
        **> wlink ()  << p <<  "click here to see it as a normal user"

    logout
   
    page  $   p <<  "the user sees the edited content. He can not edit"
        ++> tFieldEd editUser "first"  first
        **> tFieldEd editUser "second" second
        **> wlink "a"  << p <<  "click to continue"

    page  $   p <<  "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << "End of edit field demo" ++> wlink ()  << p <<  "click here to go to menu"

