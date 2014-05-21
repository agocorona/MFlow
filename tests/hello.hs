{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TransformListComp, MonadComprehensions #-}
import MFlow.Wai.Blaze.Html.All
import Data.Typeable
import Control.Monad
import Data.Time
import Data.Monoid
import MFlow.Forms.Internals
import Control.Monad.State

import Control.Workflow (exec1)


main = runNavigation "" $ transientNav. page $ do
          lazy "loading.." w
    where
    w= do
       let jqueryScript= getConfig "cjqueryScript" "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
       requires[JScriptFile jqueryScript ["alert('hello');"]]
       "hello" ++> noWidget

main3= runNavigation "" $ transientNav. page $ do
    file <- fileUpload   <** submitButton "send"
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
