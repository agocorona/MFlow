{-# LANGUAGE DeriveDataTypeable,FlexibleInstances, UndecidableInstances, OverloadedStrings, TransformListComp, MonadComprehensions #-}
import MFlow.Wai.Blaze.Html.All hiding (select)
import Data.Typeable
import Control.Monad
import Data.Time
import Data.Monoid
import MFlow.Forms.Internals
import Control.Monad.State
import Data.IORef
import Control.Workflow (exec1)
import Debug.Trace
import Data.TCache.Memoization
import System.IO.Unsafe
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Data.RefSerialize hiding ((<|>),empty)
import Data.ByteString.Char8 as B(pack)

--(!>)= flip trace



main = runNavigation "" . step $ urlShortener

data ShortUrl= ShortUrl{url :: String,  indexUrl :: Int} deriving (Read, Show, Typeable)

instance Indexable ShortUrl where key ShortUrl{url= u}= makeKey u

makeKey url= map subst url
  where
  subst '/'= '-'
  subst x = x

newtype MaxIndex= MaxIndex Int deriving (Typeable, Read, Show)

instance Indexable MaxIndex where key= const "maxIndex"

instance (Read a, Show a) => Serializable a where
   serialize= runW . showp
   deserialize= runR readp

refMaxIndex= getDBRef "maxIndex"

urlShortener= redirect <|> newEntry


redirect= page $ restp >>= findUrl
 where
 findUrl :: Int -> View Html IO ()
 findUrl ind= do
 murl <- liftIO . atomically . select url $ indexUrl .==. ind
 case murl of
  []    -> empty
  [url] -> do
     setHttpHeader "HTTP/1.1 301 Moved Permanently" ""
     setHttpHeader "Location" $ B.pack url
     return ()

newEntry= do
 liftIO $ index indexUrl  !> "newEntry"
 ind <- page $ do
    url <- getString Nothing <** submitButton "ok" <|> empty
    liftIO . atomically $ do
       let refUrl= getDBRef $ makeKey url
       found <- readDBRef refUrl
       case found of
        Nothing -> do
            MaxIndex max <- readDBRef refMaxIndex
                             `onNothing` return (MaxIndex 0)
            let max1= max + 1
            writeDBRef refUrl $ ShortUrl url max1
            writeDBRef refMaxIndex $ MaxIndex max1
            return max1
        Just (ShortUrl _ ind) -> return ind
 page $ wlink ()  << b << (mydomain <> "/" <> show ind)

mydomain= "http://localhost"

--onEmpty mx my = do
--   r <- mx
--   if r /= empty then return r
--   else my

--errorPage= "this short URL is not in the DB" ++> empty

--hideShow w= do
--  id <- genNewId
--  requires[onclick]
--  div ! onClick ("onclick("++id++")")
--  where
--  onclick= "function(id){
--    var elem= document.getElementById('html element id');
--    if(elem.style.visibility ='visible')
--       {elem.style.visibility = 'hidden'}
--    else{elem.style.visibility = 'visible'}


--ifInvalid w w'= View $ do
--    r@(FormElm _ v) <- runView w
--    case v of
--      Nothing -> runView w'
--      _ -> return r
--
--swchLink  v w= do
--  r <- restp
--  case r of
--   v -> wlink ('n':v) w !> "y"
--   ('n':v) -> wlink v w !> "n" >> empty
-- `ifInvalid` wlink v w
--
