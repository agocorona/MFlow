{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main where
import MFlow.Wai.XHtml.All
import Data.TCache
import Control.Monad.Trans
import Data.Typeable
import Control.Concurrent
import Control.Exception as E
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector as V
import Data.Maybe



data Ops= Ints | Strings | Actions | Ajax | Opt deriving(Typeable,Read, Show)
main= do
   setFilesPath ""
   addFileServerWF
   addMessageFlows [(""  ,transient $ runFlow mainf)
                   ,("shop"    ,runFlow shopCart)]
   forkIO $ run 80 waiMessageFlow
   adminLoop

stdheader c= p << "you can press the back button to go to the menu"+++ c

mainf=   do
       setHeader stdheader
       r <- ask $   wlink Ints (bold << "increase an Int")
               <|>  br ++> wlink Strings (bold << "increase a String")
               <|>  br ++> wlink Actions (bold << "Example of a string widget with an action")
               <|>  br ++> wlink Ajax (bold << "Simple AJAX example")
               <|>  br ++> wlink Opt (bold << "select options")
               <++ (br +++ linkShop) -- this is an ordinary XHtml link

       case r of
         Ints    ->  clickn 0
         Strings ->  clicks "1"
         Actions ->  actions 1
         Ajax    ->  ajaxsample
         Opt     ->  options
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


ajaxheader html= thehtml << ajaxHead << p << "click the box" +++ html



ajaxsample= do
   setHeader ajaxheader
   let ajaxf n= return $ "document.getElementById('text1').value='"++show(read  n +1)++"'"
   ajaxc <- ajaxCommand "document.getElementById('text1').value" ajaxf

   ask $ (getInt (Just 0) <! [("id","text1"),("onclick", ajaxc)])
   breturn()

actions n=do
  ask $ wlink () (p << "exit from action")
     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )
  breturn ()

-- A persistent flow  (uses step). The process is killed after 10 seconds of inactivity
-- but it is restarted automatically. if you restart the program, it remember the shopping cart
-- defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.
shopCart  = do
   setTimeouts 10 0
   shopCart1 (V.fromList [0,0,0:: Int])
   where
   shopCart1 cart=  do
     i <- step . ask $
             table ! [border 1,thestyle "width:20%;margin-left:auto;margin-right:auto"]
             <<< caption << "choose an item"
             ++> thead << tr << concatHtml[ th << bold << "item", th << bold << "times chosen"]
             ++> (tbody
                  <<<  tr ! [rowspan 2] << td << linkHome
                  ++> (tr <<< td <<< wlink  0 (bold <<"iphone") <++  td << ( bold << show ( cart V.! 0))
                  <|>  tr <<< td <<< wlink  1 (bold <<"ipad")   <++  td << ( bold << show ( cart V.! 1))
                  <|>  tr <<< td <<< wlink  2 (bold <<"ipod")   <++  td << ( bold << show ( cart V.! 2)))
                  <++  tr << td << linkHome
                  )

     let newCart= cart V.// [(i, cart V.! i + 1 )]
     shopCart1 newCart
    where
    linkHome= (toHtml $ hotlink  noScript << bold << "home")
