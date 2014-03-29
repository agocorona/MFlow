-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Forms.Cache
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
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module MFlow.Forms.Cache (
resetCachePolicy,setCachePolicy,noCache,noCache',noStore,expires,maxAge
,private, public,sMaxAge,noTransform, proxyRevalidate, etag, vary,
) where
import MFlow.Forms.Internals
import Control.Applicative
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString.Char8
import Data.List (insert,partition,sort)
import Data.Monoid


data CacheElem = Private | Public | NoCache | NoStore
               | Expires ByteString | MaxAge Int
               | SMaxAge Int | NoTransform
               | NoCache' ByteString
               | MustRevalidate | ProxyRevalidate | ETag ByteString | Vary ByteString
               deriving(Typeable, Show,Eq,Ord)

resetCachePolicy :: (MonadState (MFlowState v) m, Monad m) => m ()
resetCachePolicy= setSessionData ([] :: [CacheElem])

noCache :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noCache =  set  NoCache

noCache' :: (MonadState (MFlowState v) m, MonadIO m) => ByteString -> m ()
noCache' s =  set ( NoCache' s)

noStore :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noStore =  set NoStore

expires :: (MonadState (MFlowState v) m, MonadIO m) =>  ByteString ->  m ()
expires s =  set (Expires  s)

maxAge  :: (MonadState (MFlowState v) m, MonadIO m) =>   Int ->   m ()
maxAge t =  set (MaxAge t)

private :: (MonadState (MFlowState v) m, MonadIO m) => m ()
private =  set Private

public :: (MonadState (MFlowState v) m, MonadIO m) => m ()
public =  set Public

sMaxAge :: (MonadState (MFlowState v) m, MonadIO m) => Int -> m ()
sMaxAge secs =  set (SMaxAge secs)

noTransform :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noTransform =  set NoTransform

mustRevalidate  :: (MonadState (MFlowState v) m, MonadIO m) => m ()
mustRevalidate =  set MustRevalidate

proxyRevalidate :: (MonadState (MFlowState v) m, MonadIO m) => m ()
proxyRevalidate =  set ProxyRevalidate

etag :: (MonadState (MFlowState v) m, MonadIO m) =>  ByteString ->  m ()
etag s =  set (ETag s)

vary :: (MonadState (MFlowState v) m, MonadIO m) =>  ByteString ->  m ()
vary s =  set (Vary s)

generate ::  [CacheElem] -> [(ByteString,ByteString)]
generate []= []
generate xs = generatep xs [controlempty]
 where
 controlempty= ("Cache-Control","")
 generatep [] res= if Prelude.head res == controlempty then Prelude.tail res else res


 generatep (x:xs) ((k,v):rs) =
  case gen x of
   Right s ->  generatep xs ((k, v <> ", " <>s): rs)

   Left pair -> generatep xs (rs++[pair])

 gen NoCache= Right "no-cache"
 gen (NoCache' s)=  Right $ "no-cache= " <>s
 gen NoStore= Right "no-store"
 gen (Expires s)=  Right $ "expires= "<>s
 gen (MaxAge t)= Right $ "max-age= "<> pack (show t)
 gen Private= Right "private"
 gen Public=  Right "public"
 gen (SMaxAge t)= Right $ "s-maxage" <> pack (show t)
 gen NoTransform= Right "no-transform"
 gen MustRevalidate = Right "must-revalidate"
 gen ProxyRevalidate= Right "proxy-revalidate"
 gen (ETag s)= Left ("etag", s) :: Either (ByteString, ByteString) ByteString
 gen (Vary s)= Left ("vary",s)

set r = do
  rs <- getSessionData `onNothing` return []
  setSessionData $ r:rs



compile rs = comp' $ Data.List.sort rs
   where
   comp' []= []
   comp' [x]= [x]
   comp' z@(x:y:xs) | x==y= x:comp' xs
                    | otherwise= comp z
   comp []= []
   comp (Private:Public: xs) = Private:comp xs
   comp (NoCache:NoStore:xs)= NoCache: comp xs
   comp (NoStore: Expires _: xs)= NoStore:comp xs
   comp (NoStore:MaxAge _ : xs)= NoStore:MaxAge 0:comp xs
   comp (SMaxAge t:SMaxAge t':xs)= MaxAge (Prelude.min t t'):comp xs
   comp (Expires t:Expires t':xs)= Expires t: comp xs
   comp (Expires t:MaxAge _:xs)= Expires t: comp xs
   comp (MaxAge t:MaxAge t':xs)= MaxAge (Prelude.min t t'):comp xs

   comp (x:xs) = x: comp xs

onNothing  mmx mmy= do
  mx <- mmx
  case mx of
   Just x -> return x
   Nothing -> mmy



setCachePolicy :: (MonadState (MFlowState v) m, Monad m) => m ()
setCachePolicy= do
   rs <- getSessionData `onNothing` return  []
   (mapM_ (\(n,v) -> setHttpHeader n v ) .  generate $ compile rs)
