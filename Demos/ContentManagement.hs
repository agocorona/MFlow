
module ContentManagement ( textEdit) where
import MFlow.Wai.Blaze.Html.All
import Menu
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid

textEdit= do
    let first=  p << i <<
                   (El.span <<  "this is a page with"
                   <> b <<  " two " <> El.span <<  "paragraphs. this is the first")

        second= p << i <<  "This is the original  of the second paragraph"



    askm  $   p << b <<  "An example of content management"
        ++> first
        ++> second
        ++> wlink ()  << p <<  "click here to edit it"


    askm  $   p <<  "Please login with admin/admin to edit it"
        ++> userWidget (Just "admin") userLogin

    askm  $   p <<  "Now you can click the fields and edit them"
        ++> p << b <<  "to save an edited field, double click on it"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink ()  << p <<  "click here to see it as a normal user"

    logout

    askm  $   p <<  "the user sees the edited content. He can not edit"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink ()  << p <<  "click to continue"

    askm  $   p <<  "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << "End of edit field demo" ++> wlink ()  << p <<  "click here to go to menu"


-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav textEdit
