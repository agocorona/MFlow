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

     runNavigation "" . transientNav . page $ readEvalLoop hin hout ""

     wait $ run 80 waiMessageFlow

readEvalLoop hin hout code = do
    id <- genNewId
    () <- wpush p "append" id "id.value" $ \ cmd -> do
            wlink () << "hi"
--           out <- liftIO $ do
--              hPutStr hin code
--              hFlush hin
--              receiveLoop hout
--           unlines1 out ++> noWidget <++ br
    code <- getTextBox Nothing <! [("id",id)] <** submitButton "Enter"
    readEvalLoop hin hout code




unlines1 :: [String] -> Html
unlines1 ls= mconcat[p << l | l <- ls]





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



