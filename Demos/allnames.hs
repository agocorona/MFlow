-- | example of storage and query by using tcache
{-# LANGUAGE DeriveDataTypeable #-}
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Data.TCache.IndexText
import MFlow.Wai.Blaze.Html.All hiding(name, select)
import Data.Typeable


data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence


instance Indexable MyData where  key= name


main= do

    index name
    syncWrite  $ Asyncronous 10 defaultCheck  1000
    addMessageFlows[("", transient $ runFlow mainFlow)]
    wait $ run 80 waiMessageFlow


data Options= NewName | ListNames deriving (Show, Typeable)
mainFlow= do

     r <-ask  $  p << "menu"
             ++> wlink NewName << p << "enter a new name"
             <|> wlink ListNames << p << "get all the names"


     case r of

         NewName -> do

              name <- ask $ p << "what is your name?" ++> getString Nothing

              liftIO . atomically . newDBRef $ MyData name
              return()


         ListNames -> do
              allnames <- liftIO . atomically $ select name $   name .>. ""
              ask $ p << ("list of all names= "++ show allnames) ++> wlink ()  <<p << "click here to go to the menu"











