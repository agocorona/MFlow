{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TransformListComp, MonadComprehensions #-}
import MFlow.Wai.Blaze.Html.All
import Data.Typeable
import Control.Monad
import Data.Time
import Data.Monoid
import MFlow.Forms.Internals
import Control.Monad.State

import Control.Workflow (exec1)

--jqueryScript= getConfig "cjqueryScript" "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
--main4= runNavigation "" $ transientNav . page $ do
--    requires [JScriptFile jqueryScript ["alert('hello')"]
--             ,JScriptFile jqueryScript ["alert('hello again')"]]
--    p "hello" ++> empty


main = runNavigation "" $ step. page $ lazy "loading"
                                   (tFieldEd "editor" "hello" $ b "hello")



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
