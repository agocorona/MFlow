-- spawn GHCI and present a raw console in the browser. As a proof concept, using push and autoRefresh.
module Main where

import MFlow.Wai.Blaze.Html.All
import System.IO
import System.Process
import Data.Monoid

import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Unsafe
import Control.Concurrent.STM
import Data.Typeable


import Debug.Trace
import Data.TCache.Memoization

(!>)= flip trace

main= do
--     (Just hin, Just hout, _, _) <- 
--            createProcess (proc "ghci" [] ){ std_in= CreatePipe, std_out = CreatePipe }

     runNavigation "" $ transientNav pushSample -- $ readEvalLoop  hin hout 


pushIncrease= do
 tv <- liftIO $ newTVarIO 0
 page $ do
  push Html $ do
      setTimeouts 100 0
      n <- atomic $ readTVar tv

      atomic $ writeTVar tv $ n + 1
      liftIO $ threadDelay 1000000
      b << (show n) ++> noWidget


pushSample=  do
  tv <- liftIO $ newTVarIO $ Just "init"
  page $ push Append (disp tv) <** input tv

  where
  disp tv= do
      setTimeouts 100 0
      line <- tget tv
      p <<  line ++> noWidget

  input tv= autoRefresh $ do
      line <- getString Nothing <** submitButton "Enter"
      tput tv line


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
         push Append (do
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

--receiveLoop hout =  loop []
-- where
-- loop xs= do
--   more <- hReady  hout
--   if more
--      then do
--        x <- hGetLine hout
--        print x
--        loop $ x:xs   !> "loop"
--      else return $ concat xs  !> "next line"
--
--
--
