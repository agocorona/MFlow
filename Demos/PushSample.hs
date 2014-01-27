{-# OPTIONS   -XCPP #-}
module PushSample (pushSample) where

import Control.Concurrent.STM
import System.IO.Unsafe

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav pushSample
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif



lastLine = unsafePerformIO $ newTVarIO $ "The content will be appended here"
  
pushSample=  do
  last <- liftIO $ newTVarIO ""
  page  $ h2 << "Basic Chat as a push example"
       ++> hr
       ++> pageFlow "push" (push Append 1000 (disp last ) <** input )
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

