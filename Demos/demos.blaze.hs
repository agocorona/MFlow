{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}
module Main where
import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Widgets
import MFlow.Forms.Ajax
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String
import Data.List
import MFlow
import MFlow.FileServer
import MFlow.Forms.Ajax
import MFlow.Forms.Admin
import MFlow.Forms
import Data.TCache
import Control.Monad.Trans
import Data.Typeable

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
   addFileServerWF
   addMessageFlows [(""  ,transient $ runFlow mainmenu),
                    ("shop"    ,runFlow shopCart)]
   wait $ run 80 waiMessageFlow

--   adminLoop

stdheader c= html << El.head << body << (p << text "You can press the back button" <> c)

data Options= CountI | CountS | Radio | Login | TextEdit |Grid | Autocomp | ListEdit |Shop | Action | Ajax | Select | CheckBoxes deriving (Bounded, Enum,Read, Show,Typeable)

mainmenu=   do
       setHeader stdheader
       r <- ask $   br ++> wlink CountI   (b << text "increase an Int")
               <|>  br ++> wlink CountS   (b << text "increase a String")
               <|>  br ++> wlink Action   (b << text "Example of action, executed when a widget is validated")
               <|>  br ++> wlink Select   (b << text "select options")
               <|>  br ++> wlink CheckBoxes (b << text "checkboxes")
               <|>  br ++> wlink Radio    (b << text "Radio buttons")
               <++  br <>  br <> b << text "DYNAMIC WIDGETS"
               <|>  br ++> wlink Ajax     (b << text "AJAX example")
               <|>  br ++> wlink Autocomp (b << text "autocomplete")
               <|>  br ++> wlink ListEdit (b << text "list edition")
               <|>  br ++> wlink Grid (b << text "grid")
               <|>  br ++> wlink TextEdit (b << text "Content Management")
               <|>  br ++> wlink Shop     (b << text "example of transfer to another flow (shopping)")
               <++  br <>  br <> b << text "OTHERS"
               <|>  br ++> wlink Login    (b << text "login/logout")
               <++ (br <> a ! href  "shop" << text "shopping") -- ordinary Blaze.Html link


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
             ListEdit  ->  wlistEd
             Radio     ->  radio
             Login     ->  login
             Shop      ->  transfer "shop"



options= do
   r <- ask $ getSelect (setSelectedOption ("" :: String) (p << text "select a option") <|>
                         setOption "blue" (b << text "blue")    <|>
                         setOption "Red"  (b << text "red")  )  <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p << text " menu")

   mainmenu  -- breturn() would do it as well
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
   r <- ask $   selectAutocomplete "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu
   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

grid = do
  let row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 1)
                          <*> tdborder <<< getString (Just "hi")
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

clickn (n :: Int)= do
   setHeader stdheader
   r <- ask $  wlink ("menu" :: String) (p << text "menu")
           |+| getInt (Just n) <* submitButton "submit"
   case r of
    (Just _,_) -> mainmenu
    (_, Just n') -> clickn $ n'+1


clicks s= do
   setHeader stdheader
   s' <- ask $ (getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just "length must be < 5" else Nothing )
   clicks $ s'++ "1"

radio = do
   r <- ask $ getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]
   ask $ p << ( show r ++ " selected")  ++> wlink () (p << text " menu")
   mainmenu

ajaxsample= do
   ask $ do
         let elemval= "document.getElementById('text1').value"
         requires[JScript ajaxScript]
         ajaxc <- ajax $ \n -> return $ elemval <> "='" <> B.pack(show(read  n +1)) <> "'"
         b <<  text "click the box"
           ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc elemval)]
   mainmenu


--actions n=do
--  ask $ wlink () (p << text "exit from action")
--     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )


actions n= do
  r<- ask $   (getString $ Just "widget1") `waction` action
          <+> (getString $ Just "widget2") `waction` action
          <** submitButton "submit"
  ask $ p << ( show r ++ " returned")  ++> wlink () (p << text " menu")
  mainmenu
  where
  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum,Read, Show, Typeable)

-- A persistent flow  (uses step). The process is killed after 10 seconds of inactivity
-- but it is restarted automatically. if you restart the program, it remember the shopping cart
-- defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.
shopCart  = do
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
-- an example of content management
textEdit= do
    setHeader $ \t -> html << body << t

    let first=  p << i <<
                   (El.span << text "this is a page with"
                   <> b << text " two " <> El.span << text "paragraphs")

        second= p << i << text "This is the original text of the second paragraph"

        pageEditable =  (tFieldEd "first"  first)
                    **> (tFieldEd "second" second)

    ask $   first
        ++> second
        ++> wlink () (p << text "click here to edit it")


    ask $ p << text "Please login with admin/admin to edit it"
            ++> userWidget (Just "admin") userLogin

    ask $   p << text "now you can click the field and edit them"
        ++> p << b << text "to save the edited field, double click on it"
        ++> pageEditable
        **> wlink () (p << text "click here to see it as a normal user")

    logout

    ask $   p << text "the user sees the edited content. He can not edit"
        ++> pageEditable
        **> wlink () (p << text "click to continue")

    ask $   p << text "When text are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << text "End of edit field demo" ++> wlink () (p << text "click here to go to menu")

