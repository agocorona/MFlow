-- Modified From: https://github.com/acid-state/acid-state/blob/master/examples/HelloDatabase.hs
-- Aistis Raulinaitis
{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS -XCPP #-}
module AcidState (
acidState, initAcid
) where

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Data.SafeCopy

-- #define ALONE

#ifdef ALONE
import MFlow.Wai.Blaze.Html.All

main = do
  db <- initAcid
  runNavigation "" . step $ acidState db

#else

import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu  hiding (Database)

#endif



type Message = String
data Database = Database [Message]


initAcid= openLocalStateFrom "dist/db/" (Database [])

addMessage :: Message -> Update Database ()
addMessage msg = do
  Database messages <- get
  put $ Database (msg:messages)

viewMessages :: Int -> Query Database [Message]
viewMessages limit = do
  Database messages <- Control.Monad.Reader.ask
  return $ take limit messages

$(deriveSafeCopy 0 'Data.SafeCopy.base ''Database)
$(makeAcidic ''Database ['addMessage, 'viewMessages])


getLast10 :: AcidState (EventState AddMessage) -> IO String
getLast10 database = do
  messages <- query database (ViewMessages 10)
  return $ concat [ message ++ "  " | message <- messages ]

addMsg :: AcidState (EventState AddMessage) -> Message -> IO ()
addMsg database msg = update database (AddMessage msg)




acidState db= do
  r <- page $ h3 << "Persistent message demo."
          ++> getString Nothing
          <** submitButton "OK"

  liftIO $ addMsg db r

  last10 <- liftIO $ getLast10 db

  page $ b
    << ("You typed: "++ r ++ ", it has been added to the acid state db.")
    ++> p << ("Here are the last 10 things in the db: " ++ last10)
    ++> wlink () << p << "next"
  acidState db

