{-# OPTIONS  -XCPP #-}
module MCounter (
mcounter
) where
import Data.Monoid
import Data.String
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif




mcounter  = do 
 (op,n) <- step . page $ do
              n <- getSessionData  `onNothing` return (0 :: Int) -- get Int data from the session
              op <- h2 << show n    
                     ++> wlink "i" << b << " ++ "
                     <|> wlink "d" << b << " -- "

              return(op,n)  -- unlike in the case of the shopping example where the shopcart is not logged
                            -- here the state is smaller (the Int counter) anc can be logged      
 case op  of
          "i" -> setSessionData  (n + 1)                     
          "d" -> setSessionData  (n - 1)

 mcounter

