    {-# LANGUAGE   OverloadedStrings, DeriveDataTypeable , NoMonomorphismRestriction #-}
import MFlow.Wai.Blaze.Html.All hiding (footer, retry,step, push)
import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Control.Workflow as WF
import Control.Workflow.Stat
import Control.Concurrent.STM
import Data.Typeable
import Data.TCache.DefaultPersistence
import Data.Persistent.Collection
import Data.ByteString.Lazy.Char8(pack,unpack)
import Data.Map  as M (fromList)
import Data.List(isPrefixOf)
import Data.Maybe
import Debug.Trace
import System.IO.Unsafe
(!>) = flip trace


--comprar o reservar
--no está en stock
--reservar libro
--si está en stock pasado un tiempo quitar la reserva
--si está en stock y reservado, comprar



data Book= Book{btitle :: String, stock,reserved :: Int} deriving (Read,Show, Eq,Typeable)

instance Indexable Book where key= btitle
instance Serializable Book where
  serialize= pack. show
  deserialize= read . unpack

keyBook= "booktitle" :: String

rbook= getDBRef $  keyBook

stm= liftIO . atomically

reservetime= 5* 24 * 60 * 60  -- five days waiting for reserve and  five days reserved

data RouteOptions= Buy | Other | Reserve | NoReserve deriving (Typeable,Show)

main= do
 enterStock 30 rbook
 restartWorkflows $ M.fromList [("buyreserve",  buyReserve reservetime)]

 runNavigation "" . transientNav $ do
  op <-  page $ wlink Buy "buy or reserve the book" <++ br <|> wlink Other "Do other things"
  case op of
   Other -> page $ "doing other things" ++> wlink () "home"
   Buy -> do
     reserved  <- stm $ do
             mr <- readDBRef rbook
             case mr of
               Nothing -> return False
               Just r  ->
                 if reserved r > 0 then return True
                 else if stock r > 0 then reserveIt rbook >> return True
                 else return False

     if reserved then page $ buyIt keyBook

     else  reserveOffline keyBook



buyIt keyBook= do
      mh <- getHistory "buyreserve" keyBook

      p  "there is one book for you in stock "
         ++> case mh of
              Nothing -> p "The book was in stock and reserved online right now"
              Just hist ->
                let histmarkup= mconcat[p << l | l <- hist]
                in  h2 "History of your reserve:"
                    <> histmarkup
         ++> wlink keyBook "buy?"
                 `waction` (\keyBook -> do
                     stm $ buy rbook
                     page $  "bought! " ++> wlink () "home"
                     delWF "buyreserve" keyBook)

reserveOffline keyBook = do
     v <- getState "buyreserve" (buyReserve reservetime) keyBook
     case v of
         Left AlreadyRunning -> lookReserve keyBook
         Left err -> error $ show err
         Right (name, f, stat) -> do
           r <- page $ wlink Reserve "not in stock. Press to reserve it when available in\
                                \ the next five days. It will be reserved for five days "
                   <|> br
                   ++> wlink NoReserve "no thanks, go to home"
           case r of
             Reserve -> do



                  liftIO $ forkIO $ runWF1 name (buyReserve reservetime keyBook) stat True
                  return ()

             NoReserve -> return()


lookReserve keyBook= do
    hist <- getHistory "buyreserve" keyBook `onNothing ` return ["No workflow log"]
    let histmarkup= mconcat[p << l | l <- hist]
    page $ do
        mr <-   stm   $ readDBRef rbook
        if mr== Nothing
              || fmap stock mr == Just 0
              && fmap reserved mr  == Just 0
           then
             "Sorry, not available but you already demanded a reservation when the book\
                           \ enter in stock"
                   ++> wlink () << p "press here to go home if the book has not arrived"
                   <++ p "you can refresh or enter this url to verify availability"
                   <> h2 "status of your request for reservation upto now:"
                   <> histmarkup
           else
           h2 "Good! things changed: the book arrived and was reserved"
            ++> buyIt keyBook


buyReserve timereserve  keyBook= do
    let rbook = getDBRef keyBook
    logWF $  "You requested the reserve for: "++ keyBook
    t <- getTimeoutFlag timereserve  -- $ 5 * 24 * 60 * 60

    r <- WF.step . atomically $ (reserveAndMailIt rbook >> return True)
                      `orElse` (waitUntilSTM t >> return False)
    if not r
     then do
       logWF "reservation period ended, no stock available"
       return ()

     else do
       logWF "The book entered in stock, reserved "
       t <- getTimeoutFlag timereserve -- $ 5 * 24 *60 * 60
       r <- WF.step . atomically $ (waitUntilSTM t >> return False)
                          `orElse` (testBought rbook >> return True)

       if r
        then do
          logWF "Book was bought at this time"
        else do
          logWF "Reserved for a time, but reserve period ended"
          WF.step . atomically $ unreserveIt rbook
          return ()

userMail= "user@mail.com"

mailQueue= "mailqueue"

reserveAndMailIt rbook=  do
   let qref = getQRef mailQueue
   pushSTM qref ( userMail :: String
                , "your book "++ keyObjDBRef rbook ++ " received" :: String
                , "Hello, your book...." :: String)
   reserveIt rbook

reserveIt rbook = do
   mr <- readDBRef rbook
   case mr of
     Nothing -> retry
     Just (Book t s  r) ->
       if s >0 then writeDBRef rbook $ Book t (s-1) (r+1)
               else retry


unreserveIt rbook= do
   mr <- readDBRef rbook
   case mr of
     Nothing -> error "unreserveIt: where is the book?"
     Just (Book t s r) ->
       if r >0 then writeDBRef rbook $ Book t (s+1) (r-1)
               else return()

enterStock delay rbook= forkIO $ loop enter
  where
  loop f= f >> loop f
  enter= do
   threadDelay $ delay * 1000000
   atomically $ do
     Book _ n  r <- readDBRef rbook `onNothing` return (Book keyBook 0 0)
     writeDBRef rbook $ Book "booktitle" (n +1) r
              !> "Added 1 more book to the stock"


buy rbook=  do
   mr <- readDBRef rbook
   case mr of
     Nothing -> error "Not in stock"
     Just (Book t n n') ->
        if n' > 0  !> show mr then writeDBRef rbook $ Book t n (n'-1)
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
      Nothing -> return Nothing
      Just h  -> return  . Just
                         . catMaybes
                         . map eitherToMaybe
                         . map safeFromIDyn
                         $ versions h   ::  IO (Maybe [String])
   where
   eitherToMaybe (Right r)= Just r
   eitherToMaybe (Left _) = Nothing
