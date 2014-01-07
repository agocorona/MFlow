-- Modified From: https://github.com/acid-state/acid-state/blob/master/examples/HelloDatabase.hs

{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Main (main) where

import MFlow.Wai.Blaze.Html.All

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Data.SafeCopy

type Message = String
data Database = Database [Message]

$(deriveSafeCopy 0 'Data.SafeCopy.base ''Database)

addMessage :: Message -> Update Database ()
addMessage msg = do
  Database messages <- get
  put $ Database (msg:messages)

viewMessages :: Int -> Query Database [Message]
viewMessages limit = do
  Database messages <- Control.Monad.Reader.ask
  return $ take limit messages

$(makeAcidic ''Database ['addMessage, 'viewMessages])

getLast10 :: AcidState (EventState AddMessage) -> IO String
getLast10 database = do
  messages <- query database (ViewMessages 10)
  return $ concat [ message ++ "  " | message <- messages ]

addMsg :: AcidState (EventState AddMessage) -> Message -> IO ()
addMsg database msg = update database (AddMessage msg)

main :: IO Bool
main = do
  db <- openLocalStateFrom "dist/db/" (Database [])
  main' db

main' :: AcidState (EventState AddMessage) -> IO Bool
main' db = runNavigation "" . step $ do
  r <- page  $  h3 << "Persistent message demo."
      ++> getString Nothing
      <* submitButton "OK"

  liftIO $ addMsg db r

  last10 <- liftIO $ getLast10 db

  page $ b
    << ("You typed: "++ r ++ ", it has been added to the acid state db.")
    ++> p << ("Here are the last 10 things in the db: " ++ last10)
    ++> wlink () << p << "next"
  return ()
