
module Main where

import MFlow.Wai.Blaze.Html.All
import Data.TCache.Memoization
import System.Time
import Control.Concurrent



helloServlet=  do
      msg<- liftIO  cachedPage
      msg ++>  noWidget

seconds :: IO String
seconds= do
         TOD t _ <- liftIO getClockTime
         return $ show t

-- generates Text.Blaze.Html markup
cachedPage :: IO Html
cachedPage=cachedByKey "hello message" 10 $ do
   t<-  seconds
   return $ p << ("Hello World " ++ t)




-- helloServlet can also be written using wcached or wfreeze, which caches widgets

helloServlet'= wfreeze  "hello message Widget" 10 $ messageWidget

messageWidget ::  View Html IO ()
messageWidget= liftIO cachedPage >>= \msg -> msg ++> noWidget

--initialization



main= do

   addMessageFlows[("",wstateless helloServlet)
                   ,("alternative", wstateless helloServlet')]

   wait $ run 80 waiMessageFlow

mainConsole= do

      msg<-  cachedByKey "hello message text" 10 messageText
      print msg
      threadDelay 1000000
      main

messageText=do
   t<-  seconds
   return $ "Hello World " ++ t
