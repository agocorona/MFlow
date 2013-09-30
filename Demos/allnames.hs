-- | example of storage and query by using tcache
{-# OPTIONS -XDeriveDataTypeable  #-}
module Main where
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import MFlow.Wai.Blaze.Html.All hiding(name, select, base)
import Data.Typeable

import Data.ByteString.Lazy.Char8 as BS hiding (index)


import Control.Exception(SomeException)

data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence



instance Indexable MyData where  key=  name     -- just to notify what is the key of the register


main= do
    index name    --- index the name field, so I can query  for it later

    syncWrite  $ Asyncronous 10 defaultCheck  1000
    --  the cache will be written to files every 10 seconds. It will be cleared with the default
    --  algorithm
    --  At most 1000 objects (1000 names) will be cached simultaneously (although there will be
    --  more stored)
    runNavigation "" $ step  mainFlow



data Options= NewName | ListNames deriving (Show, Typeable)


mainFlow= do
     r <- ask $ wedit "edit" "key"
              $ p << "menu"
            ++> wlink NewName   << p << "enter a new name"
            <|> wlink ListNames << p <<  "List names"


     case r of
         NewName -> do
              name <- ask $ wedit "edit" "key3"
                          $ p << "what is your name?"
                        ++> getString Nothing
                        <** submitButton "ok"
                        
              liftIO . atomically . newDBRef $ MyData name   -- store the name in the cache (later will be written to disk automatically)
              return()

         ListNames -> do
              allnames <- liftIO . atomically $ select name $ name .>.  ""
              -- query for all the names stored in all the registers
              (ask $ p << ("list of all names= "++  show allnames) ++> wlink () << p << "click here to go to the menu")





