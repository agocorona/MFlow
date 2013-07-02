-- | example of storage and query by using tcache
{-# LANGUAGE DeriveDataTypeable #-}

import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Data.TCache.IndexText
import MFlow.Wai.Blaze.Html.All hiding(name, select)
import Data.Typeable

data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence

instance Indexable MyData where  key= name     -- just to notify what is the key of the register


main= do
    index name    --- index the name field, so I can query  it

    syncWrite  $ Asyncronous 10 defaultCheck  1000     --  the cache will be written to files every 10 seconds. It will be cleared with the default algoritm
                                                       --  At most 1000 objects (1000 names) will be cached simultaneously (although there will be more stored)
    addMessageFlows[("", transient $ runFlow mainFlow)]
    wait $ run 80 waiMessageFlow


data Options= NewName | ListNames deriving (Show, Typeable)
mainFlow= do

     r <-ask $   p << "menu"
             ++> wlink NewName   << p << "enter a new name"
             <|> wlink ListNames << p << "get all the names"

     case r of
         NewName -> do
              name <- ask $ p << "what is your name?" ++> getString Nothing
              liftIO . atomically . newDBRef $ MyData name    -- store the name in the cache (later will be written to disk automatically)
              return()

         ListNames -> do
              allnames <- liftIO . atomically $ select name $ name .>. ""        -- query for all the names stored in all the registers
              ask $ p << ("list of all names= "++ show allnames) ++> wlink () << p << "click here to go to the menu"


