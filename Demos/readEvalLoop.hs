-- spawn GHCI and present a raw console in the browser. As a proof concept, using push and autoRefresh.
module Main where

import MFlow.Wai.Blaze.Html.All
import System.IO
import System.Process
import Data.Monoid

import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad


import Debug.Trace
import Data.TCache.Memoization

(!>)= flip trace

main= do
--     (Just hin, Just hout, _, _) <- 
--            createProcess (proc "ghci" [] ){ std_in= CreatePipe, std_out = CreatePipe }

     runNavigation "" $ transientNav pushIncrease -- $ readEvalLoop  hin hout 


pushIncrease= do
 tv <- liftIO $ newTVarIO 0
 page $ do
  push Html 0 $ do
      setTimeouts 100 0   -- do nothing since the thread will kill itself.
      n <- atomic $ readTVar tv
      when (n== 100) . liftIO $ myThreadId >>= killThread
      atomic $ writeTVar tv $ n + 1
      liftIO $ threadDelay 1000000
      b << (show n) ++> noWidget


pushSample=  do
  tv <- liftIO $ newTVarIO $ Just "init"
  page $ push Append 5000 (disp tv) {-<** input tv -}<** inputAjax tv

  where
  disp tv= do
      setTimeouts 100 0
      line <- tget tv
      p <<  line ++> noWidget

--  input tv= autoRefresh $ do
--      line <- getString Nothing <** submitButton "Enter"
--      tput tv line

  inputAjax tv = do
      let elemval= "document.getElementById('text1').value"
      ajaxc <- ajax $ \line -> tput tv line >> return (fromStr "")
      getString Nothing <! [("id","text1")]
       <** submitButton "submit" <! [("onsubmit", ajaxc  elemval++"; return false;")] 



  tput tv x = atomic $ writeTVar  tv ( Just x)  !> ("PUT in " ++ addrStr tv)

  tget tv= atomic $ do
      mr <- readTVar tv
      case mr of
         Nothing -> retry
         Just r -> do
          writeTVar tv Nothing      !> ("GEt in " ++ addrStr tv)
          return r



atomic= liftIO . atomically


readEvalLoop hin hout= page $
         push Append 0 (do
                code <-  liftIO $ hGetLine hout
                p << (code :: String) ++> noWidget)

         <** getinput hin

getinput hin = autoRefresh $ do
      line <- getTextBox Nothing  <** submitButton "Enter"
      liftIO $ do
         hPutStr hin $ line++"\n"
         hFlush hin
         


unlines1 :: [String] -> Html
unlines1 ls= mconcat[p << l | l <- ls]


