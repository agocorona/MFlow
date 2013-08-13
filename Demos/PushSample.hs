
module PushSample (pushSample) where

import MFlow.Wai.Blaze.Html.All hiding (retry)
import Menu 
import Control.Concurrent.STM 


pushSample=  do
  tv <- liftIO $ newTVarIO $ Just "The content will be appended here"
  pagem $   h2 << "Push example"
       ++> p << "The content of the text box will be appended to the push widget above."
       ++> p << "A push widget can have links and form fields."
       ++> p << "Since they are asynchronous, the communucation must be trough mutable variables"
       ++> p << "The input box is configured with autoRefresh"
       ++> hr

       ++> pageFlow "push" (push Append 5000 (disp tv) <** input tv)
       **> br
       ++> br
       ++> wlink () << b << "exit"

  where
  -- the widget being pushed:
  disp tv= do
      setTimeouts 100 0
      line <- tget tv
      p <<  line ++> noWidget

  input tv= autoRefresh  $ do
      line <- getString Nothing <** submitButton "Enter"
      tput tv line


  tput tv x = atomic $ writeTVar  tv ( Just x)

  tget tv= atomic $ do
      mr <- readTVar tv
      case mr of
         Nothing -> retry
         Just r -> do
          writeTVar tv Nothing
          return r

atomic= liftIO . atomically

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav pushSample
