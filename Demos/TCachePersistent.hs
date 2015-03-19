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
--import           Database.Persist
import           Database.Persist.TH
--import           Database.Persist.Types

import           MFlow.Wai.Blaze.Html.All
--import           System.IO.Unsafe
import           Data.TCache.IndexQuery
import           Data.TCache.Defs (PersistIndex(..))
import           Data.Typeable
--import           Data.Conduit
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
    deriving Show Read Typeable
|]


instance PersistIndex (Entity a) where
  persistIndex = const Nothing

--main= do
-- runSQL $ do
--   runMigration migrateAll
--   insert $ Person "pepe" $ Just 20
-- return()


main= do
  index (blogPostAuthorId . entityVal) 
  setPersistConfig $ SqliteConf ":memory:" 10
  migratesqlite
  runNavigation "" $ transientNav tCachePersistent




migratesqlite= runSQL $ runMigration migrateAll

tCachePersistent :: FlowM Html IO ()
tCachePersistent = do
    (name, age) <- page $ (,)
                     <$> getString Nothing <! hint "your name"
                     <++ br
                     <*> getInt    Nothing <! hint "your age"
                     <** br
                     ++> submitButton "enter"


    userId <- runSQL  $ insert $ Person name $ Just age
    bool <- checkUnique $ Person name $ Just age
    liftIO $ print bool
    liftIO $ atomically $ writeDBRef $ Entity userId $ Person name $ Just age

    post <- page $ getString Nothing <! hint "your post" <** submitButton "enter"

    postId <- runSQL  $ insert $ BlogPost post userId
    liftIO $ atomically $ writeDBRef $ Entity postId $ BlogPost post userId

    oneUserPost <- liftIO $ atomically $ recordsWith
                          $ (blogPostAuthorId . entityVal :: Entity BlogPost -> Key Person)
                          .==. (userId :: Key Person)

--    oneUserPost <- runSQL  $ selectList [BlogPostAuthorId ==. userId] [LimitTo 1]

    page $ b << show (oneUserPost :: [Entity BlogPost]) ++> br ++> wlink () << b  "click here"

    let ref= getDBRef $ show userId :: DBRef (Entity Person)
    Entity _ user <- liftIO $ atomically $ readDBRef ref
                            `onNothing` error "user not found"

--    user <- runSQL $ get userId

    page $ b << show user ++> br ++> wlink ()  << b  "click here"
    where
    hint h=  [("placeholder",h)]


