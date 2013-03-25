{-# LANGUAGE OverloadedStrings,  DeriveDataTypeable #-}
module Main where
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String
import Data.List
import Data.TCache
import Data.Typeable
import Control.Monad.Trans
import Control.Concurrent
import Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid

--test= runTest [(15,"shop")]

main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath ""
   addMessageFlows
       [(""    , transient $ runFlow mainmenu)
       ,("shop", runFlow shopCart)]

   wait $ run 8081 waiMessageFlow




data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes deriving (Bounded, Enum,Read, Show,Typeable)

mainmenu=   do
       setHeader stdheader
       setTimeouts 100 0
       r <- ask $   wcached "menu" 0
                $   b << text "BASIC"
               ++>  br ++> wlink CountI       << b << text "increase an Int"
               <|>  br ++> wlink CountS       << b << text "increase a String"
               <|>  br ++> wlink Action       << b << text "Example of action, executed when a widget is validated"
               <|>  br ++> wlink Select       << b << text "select options"
               <|>  br ++> wlink CheckBoxes   << b << text "checkboxes"
               <|>  br ++> wlink Radio        << b << text "Radio buttons"
               <++  br <>  br                 <> b << text "DYNAMIC WIDGETS"
               <|>  br ++> wlink Ajax         << b << text "AJAX example"
               <|>  br ++> wlink Autocomp     << b << text "autocomplete"
               <|>  br ++> wlink AutocompList << b << text "autocomplete List"
               <|>  br ++> wlink ListEdit     << b << text "list edition"
               <|>  br ++> wlink Grid         << b << text "grid"
               <|>  br ++> wlink TextEdit     << b << text "Content Management"
               <++  br <>  br                 <> b << text "STATEFUL PERSISTENT FLOW"
                 <> br <>  a ! href "shop" << text "shopping"   -- ordinary Blaze.Html link
                 <> br <>  br <> b << text "OTHERS"

               <|>  br ++> wlink Login        << b << text "login/logout"


       case r of
             CountI    ->  clickn  0
             CountS    ->  clicks "1"
             Action    ->  actions 1
             Ajax      ->  ajaxsample
             Select    ->  options
             CheckBoxes -> checkBoxes
             TextEdit  ->  textEdit
             Grid      ->  grid
             Autocomp  ->  autocomplete1
             AutocompList -> autocompList
             ListEdit  ->  wlistEd
             Radio     ->  radio
             Login     ->  login




options= do
   r <- ask $ getSelect (setSelectedOption ("" :: String) (p << text "select a option") <|>
                         setOption "blue" (b << text "blue")    <|>
                         setOption "Red"  (b << text "red")  )  <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p << text " menu")

   mainmenu   -- breturn() would do it as well
   where
   dosummit= [("onchange","this.form.submit()")]

checkBoxes= do
   r <- ask $ getCheckBoxes(  (setCheckBox False "Red"   <++ b << text "red")
                           <> (setCheckBox False "Green" <++ b << text "green")
                           <> (setCheckBox False "blue"  <++ b << text "blue"))
              <** submitButton "submit"

   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu

autocomplete1= do
   r <- ask $   p << text "Autocomplete "
            ++> p << text "enter text"
            ++> p << text "when su press submit, the box text is returned"
            ++> wautocomplete (Just "red,green,blue") filter1
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu
   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

autocompList= do
   r <- ask $   p << text "Autocomplete with a list of selected entries"
            ++> p << text "enter text and press enter"
            ++> p << text "when su press submit, the entries are returned"
            ++> wautocompleteList "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu
   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

grid = do
  let row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 0)
                          <*> tdborder <<< getString (Just "text")
                          <++ tdborder << delLink)
      addLink= a ! href "#"
                 ! At.id "wEditListAdd"
                 << text "add"
      delLink= a ! href "#"
                 ! onclick "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode)"
                 << text "delete"
      tdborder= td ! At.style  "border: solid 1px"

  r <- ask $ addLink ++> ( wEditList table  row ["",""]) <** submitButton "submit"
  ask $   p << (show r ++ " returned")
      ++> wlink () (p << text " back to menu")

  mainmenu

wlistEd= do
   r <-  ask  $   addLink
              ++> br
              ++> (El.div `wEditList`  getString1 $  ["hi", "how are you"])
              <++ br
              <** submitButton "send"

   ask $   p << (show r ++ " returned")
       ++> wlink () (p << text " back to menu")
   mainmenu
   where
   addLink = a ! At.id  "wEditListAdd"
               ! href "#"
               $ text "add"
   delBox  =  input ! type_   "checkbox"
                    ! checked ""
                    ! onclick "this.parentNode.parentNode.removeChild(this.parentNode)"
   getString1 mx= El.div  <<< delBox ++> getString  mx <++ br

clickn n= do
   r <- ask $   p << b << text "increase an Int"
            ++> wlink ("menu" :: String) (p << text "menu")
            |+| getInt (Just n) <* submitButton "submit"
   case r of
    (Just _,_) -> mainmenu
    (_, Just n') -> clickn $ n'+1


clicks s= do
   s' <- ask $  p << b << text "increase a String"
             ++> p << b << text "press the back button to go back to the menu"
             ++>(getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just "length must be < 5" else Nothing )
   clicks $ s'++ "1"

radio = do
   r <- ask $    p << b << text "Radio buttons"
             ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]
   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu


ajaxsample= do
   r <- ask $   p << b << text "Ajax example that increment the value in a box"
            ++> do
         let elemval= "document.getElementById('text1').value"
         ajaxc <- ajax $ \n -> return $ elemval <> "='" <> B.pack(show(read  n +1)) <> "'"
         b <<  text "click the box"
           ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc . B.unpack $  elemval)]<** submitButton "submit"
   ask $ p << ( show r ++ " returned")  ++> wlink () (p << text " menu")
   mainmenu

---- recursive callbacks
--actions n=do
--  ask $ wlink () (p << text "exit from action")
--     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )


actions n= do
  r<- ask $   p << b << text "Two text boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  ask $ p << ( show r ++ " returned")  ++> wlink () (p << text " menu")
  mainmenu
  where
  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum,Read, Show, Typeable)

shopCart  = do
   setHeader $ \html -> p << (text
     "A persistent flow  (uses step). The process is killed after 10 seconds of inactivity \
     \but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n \
     \Defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.\n\
     \The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events."
     <> html)
   setTimeouts 10 0
   shopCart1 (V.fromList [0,0,0:: Int])
   where
   shopCart1 cart=  do
     o <- step . ask $
             table ! At.style "border:1;width:20%;margin-left:auto;margin-right:auto"
             <<< caption << text "choose an item"
             ++> thead << tr << ( th << b << text  "item" <> th << b << text "times chosen")
             ++> (tbody
                  <<< tr ! rowspan "2" << td << linkHome
                  ++> (tr <<< td <<< wlink  IPhone (b << text "iphone") <++  td << ( b << text (fromString $ show ( cart V.! 0)))
                  <|>  tr <<< td <<< wlink  IPod (b << text "ipad")     <++  td << ( b << text (fromString $ show ( cart V.! 1)))
                  <|>  tr <<< td <<< wlink  IPad (b << text "ipod")     <++  td << ( b << text (fromString $ show ( cart V.! 2))))
                  <++  tr << td <<  linkHome
                  )
     let i =fromEnum o
     let newCart= cart V.// [(i, cart V.!  i + 1 )]
     shopCart1 newCart

    where
    linkHome= a ! href  (fromString noScript) << b << text "home"


login= do
    ask $ p << text "Please login with admin/admin"
            ++> userWidget (Just "admin") userLogin
    user <- getCurrentUser
    ask $ b << text ("user logged as " <> T.pack user) ++> wlink () (p << text " logout and go to menu")
    logout
    mainmenu


textEdit= do

    let first=  p << i <<
                   (El.span << text "this is a page with"
                   <> b << text " two " <> El.span << text "paragraphs. this is the first")

        second= p << i << text "This is the original text of the second paragraph"



    ask $   p << b << text "An example of content management"
        ++> first
        ++> second
        ++> wlink () (p << text "click here to edit it")


    ask $   p << text "Please login with admin/admin to edit it"
        ++> userWidget (Just "admin") userLogin

    ask $   p << text "now you can click the fields and edit them"
        ++> p << b << text "to save an edited field, double click on it"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink () (p << text "click here to see it as a normal user")

    logout

    ask $   p << text "the user sees the edited content. He can not edit"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink () (p << text "click to continue")

    ask $   p << text "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << text "End of edit field demo" ++> wlink () (p << text "click here to go to menu")



stdheader c= docTypeHtml  $ body $
      a ! At.style "text-align:center" ! href ( fromString  "html/MFlow/index.html") << h1 << text "MFlow"
   <> br
   <> hr
   <> (El.div ! At.style "position:fixed;top:40px;left:0%\
                         \;width:50%;min-height:100%\
                         \;margin-left:10px;margin-right:10px" $
          h2 << text "Example of some features."
--       <> h3 << text "This demo uses warp and blaze-html"

       <> br <> c)
   <> (El.div ! At.style "position:fixed;top:40px;left:50%;width:50%;min-height:100%" $
          h2 << text "Documentation"
       <> br
       <> p  << a ! href "html/MFlow/index.html" << text "MFlow package description and documentation"
       <> p  << a ! href "demos.blaze.hs" << text "download demo source code"
       <> p  << a ! href "https://github.com/agocorona/MFlow/issues" << text "bug tracker"
       <> p  << a ! href "https://github.com/agocorona/MFlow" << text "source repository"
       <> p  << a ! href "http://hackage.haskell.org/package/MFlow" << text "Hackage repository"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/11/mflow-now-widgets-can-express.html" << text "MFlow: now the widgets can express requirements"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html" << text "On the \"spirit\" of MFlow. Anatomy of a Widget"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html" << text "MFlow active widgets example"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2013/01/stateful-but-stateless-at-last-thanks.html" << text "Stateful, but virtually stateless, thanks to event sourcing"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/11/i-just-added-some-templatingcontent.html" << text "Content Management and multilanguage in MFlow"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/10/testing-mflow-applications_9.html" << text "Testing MFlow applications"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/09/a.html" << text "A Web app. that creates Haskel computations from form responses, that store, retrieve and execute them? It´s easy"
       <> p  << a ! href "http://haskell-web.blogspot.com.es/2012/09/announce-mflow-015.html" << text "ANNOUNCE MFlow 0.1.5 Web app server for stateful processes with safe, composable user interfaces."
       )
