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

module TCachePersistent (

) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Database.Persist.Types

import           MFlow.Wai.Blaze.Html.All
import           System.IO.Unsafe
import           Data.TCache.IndexQuery
import           Data.Typeable
--import           Menu




--[Entity {entityKey = Key {unKey = PersistInt64 1}
--        , entityVal = BlogPost {blogPostTitle = "your post"
--                               , blogPostAuthorId = Key {unKey = PersistInt64 1}}}]
--
--Just (Person {personName = "pepe", personAge = Just 22})

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

instance Typeable  (Entity a)

instance Typeable SqlBackend

instance Typeable (KeyBackend backend entity)

instance (Typeable a, PersistEntity a
         ,PersistEntityBackend a ~ SqlBackend)
         => IResource (Entity a) where
  keyResource Entity{entityKey= Key (PersistInt64 x)}= show x
  writeResource Entity{..}= runSQL $ insertKey entityKey entityVal

  delResource Entity{..}= runSQL $ delete entityKey

  readResourceByKey  k = do
      let ik= read k
      mr <- runSQL $ get $ Key (PersistInt64 ik)
      case mr of
        Nothing   -> return Nothing
        Just post -> return . Just $ Entity (Key $ PersistInt64 ik) post


-- Uncomment this to run the example alone
main= do
  index entityKey
  migratesqlite
  runNavigation "" . transientNav $ mFlowPersistent

askm= ask

pool= unsafePerformIO $ createSqlitePool ":memory:" 10

runSQL :: MonadIO m => SqlPersistM a -> m a
runSQL sql= liftIO $  runSqlPersistMPool sql pool

migratesqlite= runSQL $ runMigration migrateAll

mFlowPersistent :: FlowM Html IO ()
mFlowPersistent = do

    migratesqlite              -- should be outside of the flow, in Main
    (name, age) <- askm $ (,)
                         <$> getString Nothing <! hint "your name"
                         <++ br
                         <*> getInt    Nothing <! hint "your age"
                         <** br
                         ++> submitButton "enter"

    let k=1
    let userId = Key $ PersistInt64  k
    liftIO $ atomically $ newDBRef $ Entity userId $ Person name $ Just age

--    userId <- runSQL  $ insert $ Person name $ Just age

--    post <- askm $ getString Nothing <! hint "your post" <** submitButton "enter"
--
--    let k= 1
--    liftIO $ atomically $ newDBRef $ Entity (Key $ PersistInt64  k) $ BlogPost post userId
--
----    runSQL  $ insert $ BlogPost post userId
--
--    oneUserPost <- liftIO $ atomically $ recordsWith $ entityKey .==. userId
----    oneUserPost <- runSQL  $ selectList [BlogPostAuthorId ==. userId] [LimitTo 1]
--
--    askm $ b << show (oneUserPost :: [Entity BlogPost]) ++> br ++> wlink () << b  "click here"
--
--    [Entity _ user] <- liftIO $ atomically $ recordsWith $ entityKey .==. userId
----    user <- runSQL  $ get userId
--
--    askm $ b << show user ++> br ++> wlink ()  << b  "click here"
    where
    hint h=  [("placeholder",h)]


