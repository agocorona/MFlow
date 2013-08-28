module MCounter (
mcounter
) where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid
import Data.String



mcounter  = do 
 (op,n) <- step . askm $ do
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

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation ""  mcounter


