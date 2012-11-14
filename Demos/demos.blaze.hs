{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}
module Main where
import MFlow.Hack  -- hiding (ask)
import Hack.Handler.SimpleServer
import MFlow.Forms.Blaze.Html
import MFlow.Forms.Widgets
import MFlow.Forms.Ajax
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Text.Blaze.Internal(text)
import Data.String
--import MFlow.Forms.Test
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
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid

--test= runTest [(15,"shop")]

main= do
   syncWrite SyncManual
   setFilesPath ""
   addFileServerWF
   addMessageFlows [(""  ,transient $ runFlow mainf),
                    ("shop"    ,runFlow shopCart)]
   run 80 hackMessageFlow

   adminLoop

stdheader c= html << body << (p << text "You can press the back button" <> c)

data Options= CountI | CountS | TextEdit |Shop | Action | Ajax | Select deriving (Bounded, Enum,Read, Show,Typeable)

mainf=   do
       setHeader stdheader
       r <- ask $   br ++> wlink TextEdit (b << text "Content Management")
               <|>  br ++> wlink Shop (b << text "example of transfer to another flow (shopping)")
               <|>  br ++> wlink CountI (b << text "increase an Int")
               <|>  br ++> wlink CountS (b << text "increase a String")
               <|>  br ++> wlink Action (b << text "Example of a string widget with an action")
               <|>  br ++> wlink Ajax (b << text "Simple AJAX example")
               <|>  br ++> wlink Select (b << text "select options")
               <++ (br <> linkShop) -- this is an ordinary XHtml link


       case r of
             CountI    ->  clickn 0
             CountS    ->  clicks "1"
             Action    ->  actions 1
             Ajax      ->  ajaxsample
             Select    ->  options
             TextEdit  ->  textEdit
             Shop      ->  transfer "shop"
       mainf

       where
       linkShop= a ! href  "shop" << text "shopping"

options= do
   r <- ask $ getSelect (setSelectedOption ("" :: String) (p << text "select a option") <|>
                         setOption "blue" (b << text "blue")    <|>
                         setOption "Red"  (b << text "red")  )  <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p << text " menu")
   breturn()
   where
   dosummit= [("onchange","this.form.submit()")]

clickn (n :: Int)= do
   setHeader stdheader
   r <- ask $  wlink ("menu" :: String) (p << text "menu")
           |+| getInt (Just n) <* submitButton "submit"
   case r of
    (Just _,_) -> breturn ()
    (_, Just n') -> clickn $ n'+1


clicks s= do
   setHeader stdheader
   s' <- ask $ (getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just "length must be < 5" else Nothing )
   clicks $ s'++ "1"



ajaxsample= do
   let ajaxf n= return $ "document.getElementById('text1').value='"++show(read  n +1)++"'"
   ajaxc <- ajaxCommand "document.getElementById('text1').value" ajaxf

   ask $  requires[JScript ajaxScript]
       >> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc)]
   breturn()


actions n=do
  ask $ wlink () (p << text "exit from action")
     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )
  breturn ()

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
                  <|>  tr <<< td <<< wlink  IPad (b << text "ipad")     <++  td << ( b << text (fromString $ show ( cart V.! 1)))
                  <|>  tr <<< td <<< wlink  IPod (b << text "ipod")     <++  td << ( b << text (fromString $ show ( cart V.! 2))))
                  <++  tr << td <<  linkHome
                  )
     let i =fromEnum o
     let newCart= cart V.// [(i, cart V.!  i + 1 )]
     shopCart1 newCart

    where
    linkHome= a ! href  (fromString noScript) << b << text "home"




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

    setAdminUser "admin" "admin"

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
    breturn ()
