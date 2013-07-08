module Main where

import MFlow.Wai.Blaze.Html.All
import System.IO
import System.Process
import Data.Monoid

import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Unsafe


main= do
     (Just hin, Just hout, _, _) <- 
            createProcess (proc "ghci" []){ std_in= CreatePipe, std_out = CreatePipe }

     addMessageFlows [("",wstateless $ readEvalLoop)]
     forkIO $ recloop hout
     wait $ run 80 waiMessageFlow


readEvalLoop =
    id <- geNewId
    wpush "content" "append" id "id.value" $ \ cmd -> do
           out <- liftIO $ do
              hPutStr hin code
              hFlush hin
              readBuf
           unlines1 out ++> noWidget <++ br
    loopInput
    where
    loopInput= do
       getTextBox Nothing <! [("id",id)] <** submitButton "Enter"
       loopInput

unlines1 :: [String] -> Html
unlines1 ls= mconcat[p << l | l <- ls]

bufRead= unsafePerformIO $ newMVar []

readBuf=do
          threadDelay 100000
          r <- takeMVar bufRead
          putMVar bufRead []
          return r

recloop hout = do
   l <- hGetLine hout
   print l
   ls <- takeMVar bufRead
   putMVar bufRead $ ls++[l]
   recloop  hout


receiveLoop hout = loop []
 where
 loop xs= do
   more <- hWaitForInput hout 10
   if more
      then do
        x <- hGetLine hout
        print x
        loop $ x:xs
      else return xs



