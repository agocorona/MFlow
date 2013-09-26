{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module TCachePersistent (

) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Types

import           MFlow.Wai.Blaze.Html.All
import           System.IO.Unsafe
import           Data.TCache.IndexQuery
import           Data.Typeable
import           Data.Conduit
import           Control.Monad.Logger
import           Data.TCache.Persistent.Sqlite
--import           Menu


--type PersistConf = SqliteBackend



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show Typeable
BlogPost
    title String
    authorId PersonId
    deriving Show Typeable
|]



--main= do
-- runSQL $ do
--   runMigration migrateAll
--   insert $ Person "pepe" $ Just 20
-- return()


-- Uncomment this to run the example alone
main= do
  index1 entityKey
  setPersistConfig $ SqliteConf ":memory:" 10
  migratesqlite
  runNavigation "" $ transientNav tCachePersistent

askm= ask


migratesqlite= runSQL $ runMigration migrateAll

tCachePersistent :: FlowM Html IO ()
tCachePersistent = do
    (name, age) <- askm $ (,)
                         <$> getString Nothing <! hint "your name"
                         <++ br
                         <*> getInt    Nothing <! hint "your age"
                         <** br
                         ++> submitButton "enter"

    let k=1
    let userId = Key $ PersistInt64  k
    liftIO $ atomically $ newDBRef $ Entity userId $ Person name $ Just age

    userId <- runSQL  $ insert $ Person name $ Just age

    post <- askm $ getString Nothing <! hint "your post" <** submitButton "enter"

    let k= 1
    liftIO $ atomically $ newDBRef $ Entity (Key $ PersistInt64  k) $ BlogPost post userId

--    runSQL  $ insert $ BlogPost post userId

    oneUserPost <- liftIO $ atomically $ recordsWith
                          $ entityKey .==. userId
--    oneUserPost <- runSQL  $ selectList [BlogPostAuthorId ==. userId] [LimitTo 1]

    askm $ b << show (oneUserPost :: [Entity BlogPost]) ++> br ++> wlink () << b  "click here"

--    [Entity _ user] <- liftIO $ atomically $ recordsWith $ entityKey .==. userId
--    user <- runSQL  $ get userId

--    askm $ b << show user ++> br ++> wlink ()  << b  "click here"
    where
    hint h=  [("placeholder",h)]


