-----------------------------------------------------------------------------
--
-- Module      :  Todo
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Todo (

) where

import MFlow.Wai.Blaze.Html.All
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8
import Data.Typeable

import Debug.Trace
(!>)= flip trace

newtype Todos= Todos [String] deriving (Read,Show, Typeable)

instance Indexable Todos where key= const "todos"
instance Serializable Todos where
  serialize= pack . show
  deserialize= read . unpack

rtodos= getDBRef "todos"

main= runNavigation "todo" $ step $ page $ do
     new <- ((do
              r <- getString Nothing <! [("placeholder","Enter new task here")]
              return $ Just r)
            <|> return Nothing)
     ul <<< do push "append" $ do
               case new !>  show new of
                  Nothing -> noWidget
                  Just todo -> do
                   liftIO $ atomically $ do
                    Todos todos  <- readDBRef rtodos `onNothing` return (Todos [])

                    writeDBRef rtodos  . Todos $  todo:todos
                   wraw $ li << todo
                   noWidget
