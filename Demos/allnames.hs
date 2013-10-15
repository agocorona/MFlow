-- | example of storage and query by using tcache
{-# OPTIONS -XDeriveDataTypeable  #-}
module Main where
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import MFlow.Wai.Blaze.Html.All hiding(name, select, base)
import Data.Typeable
import Data.Monoid
import Data.ByteString.Lazy.Char8 as BS hiding (index)


import Control.Exception(SomeException)

data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence



instance Indexable MyData where  key=  name     -- just to notify what is the key of the register


main= do
    index name    --- index the name field, so I can query  for it later
    syncWrite  $ Asyncronous 10 defaultCheck  1000
    userRegister "user" "user"
    runNavigation "" $ step  mainFlow



data Options= NewName | ListNames | Login deriving (Show, Typeable)


mainFlow= do
     r <- ask $ edTemplate "user" "menuallnames"
              $ wlink NewName   << p << "enter a new name"
            <|> wlink ListNames << p <<  "List names"
            <|> wlink Login << p << "login"


     case r of
         Login -> do ask  $  wlogin <|> wlink () <<p << "click to exit"
         NewName -> do
              name <- ask $ edTemplate "user" "enterallnames"
                          $ getString Nothing
                        <** submitButton "ok"

              liftIO . atomically . newDBRef $ MyData name   -- store the name in the cache (later will be written to disk automatically)
              return()

         ListNames -> do
              -- query for all the names stored in all the registers
              allnames <- liftIO . atomically $ select name $ name .>.  ""
              ask $ edTemplateList "user"  "list" (Prelude.map (wraw . p . fromStr) allnames) **> wlink () << p << "click here to go to the menu"



