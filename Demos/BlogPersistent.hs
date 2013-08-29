{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module BlogPersistent

where

import           MFlow.Wai.Blaze.Html.All
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH


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
runSQL sql= liftIO $ runSqlite ":memory:" sql
main :: IO ()
main =do
  setAdminUser "admin" "admin"
  runSqlite ":memory:" $ runMigration migrateAll

  runNavigation "" . transientNav $ do
    (name, age) <- ask $ (,) <$> getString (Just "your name") <++ br
                         <*> getInt Nothing <++ "your age"
                         <** submitButton "enter"
    userId <- runSQL $ insert $ Person name $ Just age

    post <- ask $ getString $ Just "your post"
    runSQL $ insert $ BlogPost post userId

    oneJohnPost <- runSQL $ selectList [BlogPostAuthorId ==. userId] [LimitTo 1]
    ask $ b << show (oneJohnPost :: [Entity BlogPost]) ++> wlink () << b  "click here"

    user <- runSQL $ get userId
    ask $ b << show (user :: Maybe Person) ++> wlink () << b  "click here"
  return()

