{-# OPTIONS -XQuasiQuotes #-}
module PushDecrease ( pushDecrease) where

import MFlow.Wai.Blaze.Html.All

import Control.Concurrent.STM
import Text.Hamlet
import Control.Concurrent

import Menu
-- to run it alone comment import menu and uncomment:
--main= runNavigation "" $ transientNav pushDecrease


atomic= liftIO . atomically

pushDecrease= do
 tv <- liftIO $ newTVarIO 10

 pagem $
      [shamlet|
       <div>
           <h2> Maxwell Smart push counter
           <p> This example shows a reverse counter
           <p> To avoid unnecessary load, the push process will be killed when reaching 0
           <p> The last push message will be an script that will redirect to the menu"
           <h3> This message will be autodestroyed within ...

      |] ++> counter tv <++  b << "seconds"

 where
 counter tv = push Html 0 $ do
      setTimeouts 2 0     -- kill  the thread when count finish
      n <- atomic $ readTVar tv
      if (n== 0)
        then  do
          script << "window.location='/'" ++> noWidget
        else do
          atomic $ writeTVar tv $ n - 1
          liftIO $ threadDelay 1000000
          h1 << (show n) ++> noWidget


