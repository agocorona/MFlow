
{-# LANGUAGE DeriveDataTypeable #-}

import MFlow.Wai.Blaze.Html.All hiding (footer, retry, push)
import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Control.Workflow as WF hiding(step)
import Control.Workflow.Stat
import Control.Concurrent.STM
import Data.Typeable
import Data.TCache.DefaultPersistence
import Data.Persistent.Collection
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Map  as M (fromList)
import Data.Maybe
import Debug.Trace
import System.IO.Unsafe
(!>) = flip trace

data Book= Book{btitle :: String, stock,reserved :: Int}
           deriving (Read,Show, Eq,Typeable)

instance Indexable Book where key= btitle

instance Serializable Book where
  serialize= pack. show
  deserialize= read . unpack

keyBook= "booktitle" :: String

rbook= getDBRef $  keyBook :: DBRef Book

-- show
main= do
  enterStock 10 rbook
  forkIO $ exec "buyreserve" (buyReserve  30) keyBook

  threadDelay 45000000

  putStrLn  "FINISHED"
  putStrLn "----------------WORKFLOW HISTORY:--------------"
  h <- getHistory "buyreserve" keyBook
  putStrLn $ unlines h
  putStrLn "---------------END WORKFLOW HISTORY------------"
  delWF "buyreserve" keyBook
  atomically $ delDBRef rbook


compensate :: Monad m => FlowM v m a -> FlowM v m a -> FlowM v m a
compensate doit undoit= do
     back <- goingBack
     case  back of
        False -> doit >>= breturn
        True  -> undoit

withTimeoutIO flag f  = liftIO $ atomically $ (f  >> return True)
                    `orElse` (waitUntilSTM flag >> return False)

buyReserve timereserve  keyBook= do runFlowOnce f (error "token not found") where
 f :: FlowM Html (Workflow IO) ()
 f= do
    compensate (breturn()) $ do
        lift $ logWF $ "aborting"
        error "aborted"
    let rbook = getDBRef keyBook
    lift . logWF $  "You requested the reserve for: "++ keyBook

    t <- lift $ getTimeoutFlag timereserve  -- $ 5 * 24 * 60 * 60

    r <- compensate (step . withTimeoutIO t $ reserveIt rbook)
                     (do
                       lift $ logWF "Unreserving the book"
                       step $  (liftIO . atomically $ unreserveIt rbook) >> fail "")

--     liftIO $ atomically $ (reserveIt rbook >> return True)
--                    `orElse` (waitUntilSTM t >> return False)
    if not r
     then do
       lift $ logWF "reservation period ended, no stock available"
       return ()

     else do
       lift $ logWF "The book entered in stock, reserved "
       t <- lift $ getTimeoutFlag timereserve -- $ 5 * 24 *60 * 60
       r <- step . liftIO $ atomically $ (waitUntilSTM t >> return False)
                          `orElse` (testBought rbook >> return True)

       if r
        then do
          lift $ logWF "Book was bought at this time"
        else do
          lift $ logWF "Reserved for a time, but reserve period ended"
          fail ""

--        now it is compensated above
--        step . liftIO $ atomically $ unreserveIt rbook

-- /show

reserveIt rbook = do
   mr <- readDBRef rbook
   case mr of
     Nothing -> retry
     Just (Book t s  r) ->
       if s >0 then writeDBRef rbook $ Book t (s-1) (r+1)
               else retry


unreserveIt rbook= do
   mr <- readDBRef rbook  !> "UNRESERVE"
   case mr of
     Nothing -> error "unreserveIt: where is the book?"
     Just (Book t s r) ->
       if r >0 then writeDBRef rbook $ Book t (s+1) (r-1) !> "end UNRESERVE"
               else return()

enterStock delay rbook= forkIO $ do
   liftIO $ threadDelay $ delay * 1000000
   putStrLn "ENTER STOCK"
   atomically $ writeDBRef rbook $ Book "booktitle" 5  0

buy delay rbook= forkIO $ do
  threadDelay $ delay * 1000000
  atomically $ do
   mr <- readDBRef rbook
   case mr of
     Nothing -> error "Not in stock"
     Just (Book t n n') ->
        if n' > 0 then writeDBRef rbook $ Book t n (n'-1)
                       !> "There is in Stock and reserved, BOUGHT"
        else if n > 0 then
                      writeDBRef rbook $ Book t (n-1) 0
                       !> "No reserved, but stock available, BOUGHT"
        else error "buy: neither stock nor reserve"

testBought rbook= do
    mr <- readDBRef rbook
    case mr of
       Nothing -> retry !>  ("testbought: the register does not exist: " ++ show rbook)
       Just (Book t stock reserve) ->
           case reserve  of
              0 -> return()
              n -> retry

stopRestart delay timereserve th=  do
    threadDelay $ delay * 1000000
    killThread th  !> "workflow KILLED"
    syncCache
    atomically flushAll
    restartWorkflows ( fromList [("buyreserve", buyReserve timereserve)] ) !> "workflow RESTARTED"

getHistory name x= liftIO $ do
   let wfname= keyWF name x
   let key= keyResource stat0{wfName=wfname}
   atomically $ flushKey key
   mh <- atomically . readDBRef . getDBRef $ key
   case mh of
      Nothing -> return ["No Log"]
      Just h  -> return  . catMaybes
                         . map eitherToMaybe
                         . map safeFromIDyn
                         $ versions h   :: IO [String]
   where
   eitherToMaybe (Right r)= Just r
   eitherToMaybe (Left _) = Nothing
