{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main where
import MFlow.Wai.XHtml.All  -- hiding (ask)
--import MFlow.Forms.Test
import MFlow
import MFlow.FileServer
import MFlow.Forms.Ajax
import MFlow.Forms.Admin
import MFlow.Forms
import Text.XHtml
import Data.TCache
import Control.Monad.Trans
import Data.Typeable
import Control.Concurrent
import Control.Exception as E
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V
import Data.Maybe

--test= runTest [(15,"shop")]

main= do
   syncWrite SyncManual
   setFilesPath ""
   addFileServerWF
   addMessageFlows [(""  ,transient $ runFlow mainf),
                    ("shop"    ,runFlow shopCart)]
   wait $ run 80 waiMessageFlow
--   adminLoop -- for debug

stdheader c= thehtml << body << (p << "you can press the back button to go to the menu"+++ c)

data Options= CountI | CountS | Action | Ajax | Select deriving (Bounded, Enum,Read, Show,Typeable)

mainf=   do
       setHeader stdheader
       r <- ask $   wlink CountI (bold << "increase an Int")
               <|>  br ++> wlink CountS (bold << "increase a String")
               <|>  br ++> wlink Action (bold << "Example of a string widget with an action")
               <|>  br ++> wlink Ajax (bold << "Simple AJAX example")
               <|>  br ++> wlink Select (bold << "select options")
               <++ (br +++ linkShop) -- this is an ordinary XHtml link

       case r of
             CountI    ->  clickn 0
             CountS    ->  clicks "1"
             Action    ->  actions 1
             Ajax      ->  ajaxsample
             Select    ->  options
       mainf

       where
       linkShop= toHtml $ hotlink  "shop" << "shopping"

options= do
   r <- ask $ getSelect (setSelectedOption "" (p <<"select a option") <|>
                         setOption "blue" (bold << "blue")    <|>
                         setOption "Red"  (bold << "red")  ) <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p<< " menu")
   breturn()
   where
   dosummit= [("onchange","this.form.submit()")]

clickn (n :: Int)= do
   setHeader stdheader
   r <- ask $  wlink "menu" (p << "menu")
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
  ask $ wlink () (p << "exit from action")
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
             table ! [border 1,thestyle "width:20%;margin-left:auto;margin-right:auto"]
             <<< caption << "choose an item"
             ++> thead << tr << concatHtml[ th << bold << "item", th << bold << "times chosen"]
             ++> (tbody
                  <<<  tr ! [rowspan 2] << td << linkHome
                  ++> (tr <<< td <<< wlink  IPhone (bold <<"iphone") <++  td << ( bold << show ( cart V.! 0))
                  <|>  tr <<< td <<< wlink  IPad (bold <<"ipad")     <++  td << ( bold << show ( cart V.! 1))
                  <|>  tr <<< td <<< wlink  IPod (bold <<"ipod")     <++  td << ( bold << show ( cart V.! 2)))
                  <++  tr << td << linkHome
                  )
     let i =fromEnum o
     let newCart= cart V.// [(i, cart V.!  i + 1 )]
     shopCart1 newCart

    where
    linkHome= (toHtml $ hotlink  noScript << bold << "home")
