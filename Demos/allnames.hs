-- | example of storage and query by using tcache
{-# LANGUAGE DeriveDataTypeable #-}

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import MFlow.Wai.Blaze.Html.All hiding(name, select, base)
import Data.Typeable

import Data.Serialize
import Data.SafeCopy
import Data.ByteString.Lazy.Char8 as BS hiding (index)

data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence

instance SafeCopy MyData where
   putCopy (MyData name)= contain $ safePut name
   getCopy = contain $ MyData <$> safeGet
   version = 1
   kind = base


instance Serializable MyData where
   serialize= strictB2Lazy . runPut . migrate. putCopy
   deserialize = runGet getCopy . lazyB2Strict

lazyB2Strict= BS.concat . BS.toChunks
strictB2Lazy= BS.fromChunks . []

instance Indexable MyData where  key= name     -- just to notify what is the key of the register


main= do
    index name    --- index the name field, so I can query  for it later

    syncWrite  $ Asyncronous 10 defaultCheck  1000
    --  the cache will be written to files every 10 seconds. It will be cleared with the default
    --  algorithm
    --  At most 1000 objects (1000 names) will be cached simultaneously (although there will be
    --  more stored)
    addMessageFlows[("", transient $ runFlow mainFlow)]
    wait $ run 80 waiMessageFlow


data Options= NewName | ListNames deriving (Show, Typeable)
mainFlow= do

     r <- ask $   p << "menu"
             ++> wlink NewName   << p << "enter a new name"
             <|> wlink ListNames << p << "get all the names"

     case r of
         NewName -> do
              name <- ask $ p << "what is your name?" ++> getString Nothing
              liftIO . atomically . newDBRef $ MyData name    -- store the name in the cache (later will be written to disk automatically)
              return()

         ListNames -> do
              allnames <- liftIO . atomically $ select name $ name .>. ""
              -- query for all the names stored in all the registers
              ask $ p << ("list of all names= "++ show allnames) ++> wlink () << p << "click here to go to the menu"


