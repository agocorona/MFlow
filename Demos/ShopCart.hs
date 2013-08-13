
{-# OPTIONS  -XDeriveDataTypeable #-}
module ShopCart ( shopCart) where

import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Typeable
import qualified Data.Vector as V
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.String
import Data.Typeable

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum, Show,Read , Typeable)


newtype Cart= Cart (V.Vector Int) deriving Typeable
emptyCart= Cart $ V.fromList [0,0,0]

shopCart  = do
   setTimeouts 120 0
   setHeader stdheader
   addHeader $ \html -> p << ( El.span <<
     "A persistent flow  (uses step). The process is killed after 100 seconds of inactivity \
     \but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n \
     \Defines a table with links that return ints and a link to the menu, that abandon this flow.\n\
     \The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events."

     <> html)
   setTimeouts 100 (60 * 60)
   shopCart1
   where
   shopCart1 =  do
     o <-  step . askm $ do
             let moreexplain= p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"
             Cart cart <- getSessionData `onNothing` return  emptyCart

             moreexplain
              ++>
              (table ! At.style (attr "border:1;width:20%;margin-left:auto;margin-right:auto")
              <<< caption <<  "choose an item"
              ++> thead << tr << ( th << b <<   "item" <> th << b <<  "times chosen")
              ++> (tbody
                  <<< tr ! rowspan (attr "2") << td << linkHome
                  ++> (tr <<< td <<< wlink  IPhone (b <<  "iphone") <++  td << ( b <<  show ( cart V.! 0))
                  <|>  tr <<< td <<< wlink  IPod   (b <<  "ipod")   <++  td << ( b <<  show ( cart V.! 1))
                  <|>  tr <<< td <<< wlink  IPad   (b <<  "ipad")   <++  td << ( b <<  show ( cart V.! 2)))
                  <++  tr << td <<  linkHome
                  ))
     let i =fromEnum o
     Cart cart <- getSessionData `onNothing` return emptyCart
     setSessionData . Cart $ cart V.// [(i, cart V.!  i + 1 )]
     shopCart1

    where
    linkHome= a ! href  (attr $ "/" ++ noScript) << b <<  "home"
    attr= fromString

-- to run it alone:
--main= runNavigation ""   shopCart
