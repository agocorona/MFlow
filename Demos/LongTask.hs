-----------------------------------------------------------------------------
--
-- Module      :  LongTask
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
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}
module LongTask () where

import MFlow.Wai.Blaze.Html.All
import Control.Concurrent.MVar
import Control.Concurrent
import Debug.Trace
import Data.Typeable
import Data.String


(!>) = flip trace

main= runNavigation "longtask" . step $ pushtest [ p << "hello" ++> noWidget
                                                 , b << "word" ++> noWidget]


data PushList view m a = PushList [View view m ()]

instance (Typeable view)
         =>Typeable (PushList view m a) where
  typeOf= \v -> mkTyConApp (mkTyCon3 "MFlow" "MFlow.Forms.Widgets" "PushList" ) []



pushtest fss=do
   page $ push Html 0  $ do
         PushList fs <- getSessionData  `onNothing` return ( PushList fss)
         if null fs  !> (show $ length fs) then fail "" else do
            setSessionData . PushList $ tail fs
            liftIO $ threadDelay 1000000
            Prelude.head fs





pr= do
  threadDelay 10000000
  return "hello"

data Waiting = Waiting deriving Typeable

longTask task showresult timeout= do
   tv <- liftIO $ newEmptyMVar
   r <- do
          th <- liftIO $ forkIO $ task >>= putMVar tv
          setSessionData th
        `compensate`
        do
          mth <- getSessionData
          case mth of
            Nothing -> return()
            Just th -> liftIO $ killThread th
   page $ push Html 0 (do
           setTimeouts timeout 0
           mr <- getSessionData
           case mr of
             Nothing -> do
               setSessionData Waiting
               b << "please wait" ++> noWidget
             Just Waiting -> do
               result <- liftIO $ takeMVar tv
               delSessionData Waiting
               delSessionData th
               p << showresult result
                ++> ((wlink () << "press to exit") <! noAutoRefresh))


