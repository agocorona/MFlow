{-# LANGUAGE DeriveDataTypeable #-}

import Control.Workflow as WF
import Data.TCache
import Data.TCache.DefaultPersistence
import Control.Concurrent.STM
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Typeable
import Control.Concurrent(forkIO,threadDelay, killThread)
import Control.Monad.IO.Class(liftIO)
import Control.Workflow.Stat
import Data.Maybe
import Debug.Trace
import Data.Map (fromList)

(!>)= flip trace

data Book= Book{btitle :: String, stock, price,reserved :: Int} deriving (Read,Show, Eq,Typeable)

instance Indexable Book where key= btitle

instance Serializable Book where
  serialize= pack. show
  deserialize= read . unpack


main= do
  putStrLn "FIRST CASE: the stock appears at 20 seconds, is bought at 40. the timeouts (100) are not reached."
  test 20  40 100 30
  putStrLn "press a letter to start the second case"
  getChar

  putStrLn "SECOND CASE: the stock appears at 20, and it is bought at 115, after the end of the reserve (20+110)"
  test 20 115 100 50
  putStrLn "wait for the BUY message and press a letter to start the third case"
  getChar

  putStrLn "THIRD CASE: the product enter in stock when the reervation period was finished, but the buyer appears\
           \shortly after and buy the product."

  test 120 130 100 30
  putStrLn "press a letter to end the the third case"
  getChar


test stockdelay buydelay timereserve stopdelay = do
  let keyBook= "booktitle"
      rbook= getDBRef  keyBook

  enterStock stockdelay rbook
  buy buydelay rbook

  th <- fork $ exec "buyreserve" (buyReserve  timereserve) keyBook
  stopRestart stopdelay timereserve th

  putStrLn  "FINISED"
  atomically $ delDBRef rbook
  putStrLn "----------------WORKFLOW HISTORY:--------------"
  h <- getHistory "buyreserve" keyBook
  putStrLn $ unlines h
  putStrLn "--------------END WORKFLOW HISTORY:------------"




buyReserve timereserve  keyBook= do
    let rbook = getDBRef keyBook
    logWF $  "Reserve workflow start for: "++ keyBook
    t <- getTimeoutFlag timereserve -- $ 5 * 24 * 60 * 60

    r <- WF.step . atomically $ (reserveIt rbook >> return True)
                       `orElse` (waitUntilSTM t >> return False)
    if not r
     then do
       logWF "reservation period ended, no stock available"
       return ()

     else do
       logWF "The book entered in stock, reserved "
       t <- getTimeoutFlag timereserve -- $ 5 * 24 *60 * 60
       r <- WF.step . atomically $ (waitUntilSTM t >> return False)
                          `orElse` (testBought rbook   >> return True)

       if r
        then do
          logWF "Book was bought at this time"
        else do
          logWF "Reserved for a time, but reserve period ended"
          WF.step . atomically $ unreserveIt rbook
          return ()



reserveIt rbook = do
   mr <- readDBRef rbook
   case mr of
     Nothing -> retry
     Just (Book t s p r) -> writeDBRef rbook $ Book t (s-1) p (r+1)


unreserveIt rbook= do
   mr <- readDBRef rbook
   case mr of
     Nothing -> error "where is the book?"
     Just (Book t s p r) -> writeDBRef rbook $ Book t (s+1) p (r-1)

enterStock delay rbook= forkIO $ do
   liftIO $ threadDelay $ delay * 1000000
   putStrLn "ENTER STOCK"
   atomically $ writeDBRef rbook $ Book "booktitle" 5 10 0

buy delay rbook= forkIO $ do
   liftIO $ threadDelay $ delay * 1000000
   putStrLn "BUY"
   atomically $ do
       mr <- readDBRef rbook
       case mr of
           Nothing -> retry
           Just (Book t stock p reserve) ->
               case (stock, reserve) of
                   (0,0)  ->  retry

                   (0,n)  -> writeDBRef rbook $ Book t 0 p (n-1)

                   (n,n')  ->
                        if n' >0
                           then writeDBRef rbook $ Book t n p (n'-1)
                           else writeDBRef rbook $ Book t (n-1) p 0



testBought rbook= do
    mr <- readDBRef rbook
    case mr of
       Nothing -> error $ "bought: the register does not exist: " ++ show rbook
       Just (Book t stock p reserve) ->
           case reserve  of
              0 -> return()
              n -> retry

stopRestart delay timereserve th= do
    threadDelay $ delay * 1000000
    killThread th !> "workflow KILLED"
    restartWorkflows $ fromList [("buyreserve", buyReserve timereserve)]  !> "RESTARTED"

getHistory name x= liftIO $ do
   let wfname= keyWF name x
   mh <- atomically . readDBRef . getDBRef $ keyResource stat0{wfName=wfname}
   case mh of
      Nothing -> error  $ "history does not exist "++ keyWF name x
      Just h  -> return  . reverse
                         . catMaybes
                         . map eitherToMaybe
                         . map safeFromIDyn
                         $ versions h
   where
   eitherToMaybe (Right r)= Just r
   eitherToMaybe (Left _) = Nothing
