{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TransformListComp, MonadComprehensions #-}
import MFlow.Wai.Blaze.Html.All
import Data.Typeable
import Control.Monad
import Data.Time
import Data.Monoid
import MFlow.Forms.Internals
import Control.Monad.State
import Data.IORef
import Control.Workflow (exec1)
import Debug.Trace
import Data.TCache.Memoization
import System.IO.Unsafe

(!>)= flip trace



main7 = runNavigation "" $ step. page $ lazy "loading"
                                   (tFieldEd "editor" "hello" $ b "hello")

--hideShow w= do
--  id <- genNewId
--  requires[onclick]
--  div ! onClick ("onclick("++id++")")
--  where
--  onclick= "function(id){
--    var elem= document.getElementById('html element id');
--    if(elem.style.visibility ='visible')
--       {elem.style.visibility = 'hidden'}
--    else{elem.style.visibility = 'visible'}


ifInvalid w w'= View $ do
    r@(FormElm _ v) <- runView w
    case v of
      Nothing -> runView w'
      _ -> return r

swchLink  v w= do
  r <- restp
  case r of
   v -> wlink ('n':v) w !> "y"
   ('n':v) -> wlink v w !> "n" >> empty
 `ifInvalid` wlink v w

main= runNavigation "" . step . page $ do
     (,) <$> getInt Nothing
         <*> getInt Nothing
         <** submitButton "enter"
     return ()


main3= runNavigation "" $ transientNav. page $ do
    file <- fileUpload <** submitButton "send"
    p <<  show file ++> wlink () " again"

main2= runNavigation "showResults" $ transientNav $ do
    page $ p  "Lazy present the 10 p" ++> empty
    r <- page $  lazyPresent  (0 :: Int) 10
    page $ wlink ("jj"  :: String) << p << show r
    return ()


lazyPresent i n=  firstOf[  lazy "loading..." (wlink i << p << (show i) ) | i <- [i..n :: Int]]

lazyPresentR i n
   | i == n= noWidget
   | otherwise= wlink i << p << (show i) <|> lazy "loading..." (lazyPresentR (i+1) n)
