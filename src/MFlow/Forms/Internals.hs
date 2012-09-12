-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Forms.Internals
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
{-# OPTIONS  -XDeriveDataTypeable
             
#-}

module MFlow.Forms.Internals where

import Data.RefSerialize
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Data.ByteString.Lazy.Char8
import Data.Typeable



newtype Lang= Lang String

data MFlowState view= MFlowState{   
   mfSequence :: Int,
   mfCached  :: Bool,
   prevSeq    :: [Int],
   onInit     :: Bool,
--   mfGoingBack :: Bool,
   validated   :: Bool,
--   mfUser     :: String,
   mfLang     :: Lang,
   mfEnv      :: Params,
   needForm   :: Bool,
   hasForm    :: Bool,
--   mfServer   :: String,
--   mfPath     :: String,
--   mfPort     :: Int,

   mfToken     :: Token,
   mfkillTime :: Int,
   mfSessionTime :: Integer,
   mfCookies   :: [Cookie],
   mfHeader ::  view -> view,
   mfDebug  :: Bool
   }

   deriving Typeable
