{-# LANGUAGE   OverloadedStrings, DeriveDataTypeable , NoMonomorphismRestriction #-}
import MFlow.Wai.Blaze.Html.All hiding (footer, retry,step)
import MFlow.Forms.Internals
import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Control.Workflow as WF
import Control.Workflow.Stat
import Control.Concurrent.STM
import Data.Typeable
import Data.TCache.DefaultPersistence
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



data Book= Book{btitle :: String, stock, price,reserved :: Int} deriving (Read,Show, Eq,Typeable)

instance Indexable Book where key= btitle
instance Serializable Book where
  serialize= pack. show
  deserialize= read . unpack

keyBook= "Bible" :: String

rbook= getDBRef $  keyBook

stm= liftIO . atomically

data RouteOptions= Buy | LookReserve | Reserve | NoReserve deriving (Typeable,Show)

main= do
 enterStock
 restartWorkflows $ M.fromList [("buyreserve",  buyReserve)]
 runNavigation "" . transientNav $ do
  op <- page $ wlink Buy "buy a book" <++ br <|> wlink LookReserve "Look for my reservation"
  case op of

   Buy -> do
     mr <- stm $ readDBRef rbook
     case mr== Nothing || fmap stock mr == Just 0 of
       True -> do
         r <- page $ wlink Reserve "not in stock. Do you want to reserve when available in the next five days? "
                 <|> br
                 ++> wlink NoReserve "no thanks"

         case r of
           Reserve -> do
              liftIO $ forkIO $ exec "buyreserve"  buyReserve keyBook
              return()
       False -> page $ buy **> wlink () "bought!"


   LookReserve -> do
        hist <- getHistory "buyreserve" keyBook
        page $ do
            mr <-   stm   $ readDBRef rbook  !> "readResource thebook"
            case mr of
              Nothing ->  "No reservations " ++> wlink () "home"
              Just (Book t s p n) ->
                 if n > 0 then do
                    "the book is in stock and reserved for you. "++> wlink () "Buy?"
                       <++ br
                       <> hist
                    buy
                 else if s >0 then do
                    stm $  reserve rbook
                    "not reserved, but stock available" ++>br
                       ++>  hist
                       ++> wlink  () "buy?" `waction`  const (page buy)

                 else do
                    wlink () "sorry, not available"
                       <++   hist


getHistory name x= liftIO $ do
   mh <- getResource Stat{wfName= keyWF name x}
   case mh of
      Just h -> return  $ do
                    "For more information about the book "
                    mconcat
                     [p << (x ::String)
                     | x <- reverse
                             . catMaybes
                             . map eitherToMaybe
                             . map safeFromIDyn
                             $ versions h]



      Nothing -> return "no history"
   where
   eitherToMaybe (Right r)= Just r
   eitherToMaybe (Left _) = Nothing

inStock _= False

buy=  stm $ do
  mbook <-  readDBRef rbook
  case mbook of
   Just book -> writeDBRef rbook book{stock=stock book -1,reserved= reserved book -1}
   Nothing -> error "where my book was?"
  return()




enterStock= forkIO $ do
   liftIO $ threadDelay 20000000
   writeDBRef rbook Book "Bible" 5 10 0


buyReserve :: String -> Workflow IO ()
buyReserve keyBook= do
    let rbook = getDBRef keyBook
    logWF $  "waiting stock"
    t <- getTimeoutFlag $ 5 * 24 * 60 * 60

    r <- WF.step . atomically $ reserve rbook >> return True
                       `orElse` (waitUntilSTM t >> return False)
    False -> do
       logWF "reservation period ended, no stock available"
       return ()

    True  -> do
       logWF "The book entered in stock, reserved "
       t <- getTimeoutFlag $ 5 * 24 *60 * 60
       WF.step . atomically $ waitUntilSTM t

       logWF "entered in stock but reservation period ended"

       WF.step . atomically $ unreserve rbook
       return ()

reserve rbook = do
   mr <- readDBRef rbook
   case mr of
     Nothing -> retry
     Just (Book t s p r) -> writeDBRef rbook $ Book t (s-1) p (r+1)


unreserve rbook= do
   mr <- readDBRef rbook
   case mr of
     Nothing -> error "where is the book?"
     Just (Book t s p r) -> writeDBRef rbook $ Book t (s-1) p (r+1)


