{-# OPTIONS -XQuasiQuotes  -XCPP #-}
module PushDecrease ( pushDecrease) where

import Control.Concurrent.STM
import Text.Hamlet
import Control.Concurrent

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav pushDecrease
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif


atomic= liftIO . atomically

pushDecrease= do
 tv <- liftIO $ newTVarIO 10

 page $
      [shamlet|
       <div>
           <h2> Maxwell Smart push counter
           <p>  This example shows a reverse counter
           <p>  To avoid unnecessary load, the push process will be killed when reaching 0
           <p>  The last push message will be an script that will redirect to the menu"
           <h3> This message will be autodestroyed within ...

      |] ++> counter tv <++  b << "seconds"

 where
 counter tv = pageFlow "push" . push Html 0 $ do
      setTimeouts 2 0     -- kill  the thread after 2 s of incactivity, when count finish
      n <- atomic $ readTVar tv
      if (n== -1)
        then  do
          script << "window.location='/'" ++> noWidget
        else do
          atomic $ writeTVar tv $ n - 1
          liftIO $ threadDelay 1000000
          h1 << (show n) ++> noWidget


