
module PushSample (pushSample) where

import MFlow.Wai.Blaze.Html.All hiding (retry)
import Menu 
import Control.Concurrent.STM
import System.IO.Unsafe
import Debug.Trace
(!>)= flip trace

lastLine = unsafePerformIO $ newTVarIO $ "The content will be appended here"
  
pushSample=  do
  last <- liftIO $ newTVarIO ""
  pagem  $ h2 << "Basic Chat as a push example"
       ++> hr
       ++> pageFlow "push" (push Append 0 (disp last ) <** input )
       **> br
       ++> br
       ++> wlink () << b << "exit"

  where
  -- the widget being pushed:
  disp last = do
      setTimeouts 100 0  -- after 100 seconds of inactivity , kill itself, but restart if needed
      line <- tget last
      p <<  line ++> noWidget

  input = autoRefresh  $ do
      line <- getString Nothing <** submitButton "Enter"
      tput lastLine line


  tput tv x = atomic $ writeTVar  tv  x

  tget last = atomic $ do
      msg <- readTVar lastLine
      lastmsg <- readTVar last
      case lastmsg == msg of
         False ->  do
            writeTVar last msg
            return msg
         True -> retry
           

atomic= liftIO . atomically

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav pushSample
