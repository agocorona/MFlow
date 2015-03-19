{-# OPTIONS  -XDeriveDataTypeable -XCPP #-}
module ShopCart ( shopCart) where
import Data.Typeable
import qualified Data.Vector as V
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.String
import Data.Typeable
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum, Show,Read , Typeable)

newtype Cart= Cart (V.Vector Int) deriving Typeable

emptyCart= Cart $ V.fromList [0,0,0]

shopCart= shopCart1

shopCart1  =  do
--     setHeader  stdheader 
--     setTimeouts 200 $ 60*60   
     prod <-
        step . page $ do
             Cart cart <- getSessionData `onNothing` return  emptyCart

             moreexplain
              ++> 
              (table ! At.style (attr "border:1;width:20%;margin-left:auto;margin-right:auto")
              <<< caption <<  "choose an item"
              ++> thead << tr << ( th << b <<   "item" <> th << b <<  "times chosen")
              ++> (tbody
                  <<< tr ! rowspan (attr "2") << td << linkHome
                  ++> (tr <<< td <<< wlink  IPhone (b <<  "iphone")
                          <++  tdc << ( b <<  show ( cart V.! 0))
                  <|>  tr <<< td <<< wlink  IPod   (b <<  "ipod")
                          <++  tdc << ( b <<  show ( cart V.! 1))
                  <|>  tr <<< td <<< wlink  IPad   (b <<  "ipad")
                          <++  tdc << ( b <<  show ( cart V.! 2)))
                  <++  tr << td <<  linkHome
                  ))

                  
     let i = fromEnum prod
     Cart cart <- getSessionData `onNothing` return  emptyCart
     setSessionData . Cart $ cart V.// [(i, cart V.!  i + 1 )]
     shopCart1

    where
    tdc= td ! At.style (attr "text-align:center")
    linkHome= a ! href  (attr $ "/" ++ noScript) << b <<  "home"
    attr= fromString
    moreexplain= do
     p $ El.span <<(
         "A persistent flow  (uses step). The process is killed after the timeout set by setTimeouts "++
         "but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart "++
         " The shopping cart is not logged, just the products bought returned by step are logged. The shopping cart "++
         " is rebuild from the events (that is an example of event sourcing."++
         " .Defines a table with links that return ints and a link to the menu, that abandon this flow. "++
         " .The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events.")

     p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"
