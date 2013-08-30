-- This example ilustrate the use of MFlow with Persistent
-- The code is taken from http://www.yesodweb.com/book/persistent by modifying the first example
--
-- The example has a navigation of four pages and you can go forward and backward
-- Note how little additions are necessary to change a console oriented application to a Web
-- application with MFlow. While the flow looks like an ordinary imperative program, yo can go
-- back and forth and to introduce any bookmark without producing navigation errors.
--
-- You can press the back button, change the form input and see how the responses match
-- the expected register values.

{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module MFlowPersistent

where

import           MFlow.Wai.Blaze.Html.All
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           System.IO.Unsafe

import           Menu



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

-- Uncomment this to run the example alone
--main= do
--  migratesqlite
--  runNavigation "" . transientNav $ mFlowPersistent
--
--askm= ask


pool= unsafePerformIO $ createSqlitePool ":memory:" 10

runSQL sql= liftIO $  runSqlPersistMPool sql pool

migratesqlite= runSQL $ runMigration migrateAll

mFlowPersistent :: FlowM Html IO ()
mFlowPersistent = do
    migratesqlite              -- should be outside of the flow, in Main
    (name, age) <- askm $ (,)
                         <$> getString Nothing <! hint "your name" <++ br
                         <*> getInt    Nothing <! hint "your age"
                         <** br
                         ++> submitButton "enter"

    userId <- runSQL  $ insert $ Person name $ Just age

    post <- askm $ getString $ Just "your post"
    runSQL  $ insert $ BlogPost post userId

    oneUserPost <- runSQL  $ selectList [BlogPostAuthorId ==. userId] [LimitTo 1]

    askm $ b << show (oneUserPost :: [Entity BlogPost]) ++> br ++> wlink () << b  "click here"

    user <- runSQL  $ get userId

    askm $ b << show (user :: Maybe Person) ++> br ++> wlink ()  << b  "click here"
    where
    hint h=  [("placeholder",h)]


