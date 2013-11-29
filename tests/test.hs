{-# LANGUAGE   OverloadedStrings, DeriveDataTypeable #-}
import MFlow.Wai.Blaze.Html.All hiding (footer)
import MFlow.Forms.Internals
import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Control.Workflow hiding (step)
import Data.Typeable
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8

import Debug.Trace

(!>) = flip trace

footer= a ! href "http://haskell.org"  $ img ! src "haskell-logo-revolution.png"
main2= runNavigation "" $ step $ do
  setFilesPath "tests/"
  ask $ wlink () "press here" <++ footer
  r <- ask $ getString Nothing
  ask $ wlink () (b << r)  <++ footer

--comprar o reservar
--no está en stock
--reservar libro
--si está en stock pasado un tiempo quitar la reserva
--si está en stock y reservado, comprar



main= do
 addMessageFlows [("buyreserve", runFlow buyReserve)]
 runNavigation ""  . step $ do
  page $ (a ! href "/buyreserve" $ "buy a book") ++> br ++> wlink () "other"
  page $ wlink () "doing other things"

inStock _= False

data Book= Book{btitle :: String, stock, price,reserved :: Int} deriving (Read,Show,Typeable)

instance Indexable Book where key= btitle
instance Serializable Book where
  serialize= pack. show
  deserialize= read . unpack

buy= undefined

buyReserve= do
 let thebook= Book{btitle= "Bible"}
 if inStock thebook then buy
  else do
   r <- step $ nobacktrack $ ask $ wlink True "not in stock. Reserve when available? "
                 <|> br
                 ++> wlink False " no thanks"
   if r then do
        step $ do
            nobacktrack $ page $  "waiting stock" ++> noWidget <++ (a ! href "/" $ "home")
            liftIO $ forkIO $ waitStock thebook
            mr <- liftIO $ readResource thebook
            case mr of
              Nothing -> nobacktrack $ page $  "not in Stock yet" ++> noWidget <++ (a ! href "/" $ "home")
              Just (Book t s p 0) ->
                 if s >0 then do
                    liftIO . atomically $ withSTMResources[thebook] reserve
                    page $ wlink () "reservation period ended, but stock available"
                 else do
                    page $ wlink () "sorry reservation period ended. Not available again"


        buyReserve

    else
      step $ page $ wlink () "ok, do not reserve"

addStock= do
   liftIO $ threadDelay 100000000
   withResources [] $ const [Book "Bible" 5 10 0]



waitStock thebook= do
    atomically $ withSTMResources[ thebook] reserve
    threadDelay 100000000
    atomically $ withSTMResources[thebook] unreserve

reserve [Nothing]= Retry
reserve [Just(Book t s p r)] = resources{toAdd=[Book t (s-1) p (r+1)]}

unreserve [Nothing]= resources{toReturn= ()}
unreserve [Just(Book t s p r)] =  resources{toAdd=[Book t (s+1) p (r-1)]}


nobacktrack msg= do
   back <- goingBack
   if not back  then msg else do
             breturn()  -- will not go back beyond this
             clearEnv
             modify $ \s -> s{newAsk= True}
             breturn r
