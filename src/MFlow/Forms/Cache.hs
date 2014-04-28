-----------------------------------------------------------------------------
-- | Composable cache and HTTP header directives.
-- Intended to permit each widget to express his caching needs to the whole page
-- The page will compile them and choose the most strict ones
-- Autorefreshed, push and witerate'd widgets do not inherit the page rules. they must specify
-- their own.
--
-- The composition rules are explained in the corresponding combinators. This is a work in progress
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable,FlexibleContexts, OverloadedStrings #-}
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
               | MustRevalidate | ProxyRevalidate
               | ETag ByteString | Vary ByteString
               deriving(Typeable, Show,Eq,Ord)

-- | to delete all previous directives
resetCachePolicy :: (MonadState (MFlowState v) m, Monad m) => m ()
resetCachePolicy= do
  modify $ \s -> s{mfHttpHeaders=[]}
  setSessionData ([] :: [CacheElem])

-- | add @no-cache@ to the @Cache-Control@ header directive. It deletes all expires and put max-age=0
--
-- It means that the widget need not to be cached
noCache :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noCache =  set  NoCache

-- | add @no-cache: <string>@  to the Cache-Control header directive
--
-- it deletes the header string (sensible cookies for example) from the data stored in the cache
noCache' :: (MonadState (MFlowState v) m, MonadIO m) => ByteString -> m ()
noCache' s =  set ( NoCache' s)

-- | add @no-store@ to the @Cache-Control@ header directive. It deletes  @expires@ and put @max-age: 0@
--
-- stronger kind of noCache. Not even store temporally
noStore :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noStore =  set NoStore

-- | add @expires: <date string>@ to the @Cache-Control@ header directive. it deletes @max-age@
-- Currently it takes the last one if many
--
-- The page will be cached until this date
expires :: (MonadState (MFlowState v) m, MonadIO m) =>  ByteString ->  m ()
expires s =  set (Expires  s)

-- | add @max-age: <seconds>@ to the @Cache-Control@ header directive. if there are more than one, it chooses the lower one
--
-- The page will be stored in the cache for that amount of seconds
maxAge  :: (MonadState (MFlowState v) m, MonadIO m) =>   Int ->   m ()
maxAge t =  set (MaxAge t)

-- | add @private@ to the @Cache-Control@ header directive. it delete @public@ if any
--
-- It means that the page that holds the widget must not be shared by other users.
private :: (MonadState (MFlowState v) m, MonadIO m) => m ()
private =  set Private

-- | add @public@ to the @Cache-Control@ header directive.
--
-- means that the cache can share the page content with other users.
public :: (MonadState (MFlowState v) m, MonadIO m) => m ()
public =  set Public

-- | add @sMaxAge <seconds>@ to the @Cache-Control@ header directive. if many, chooses the minimum
--
-- specify the time to hold the page for intermediate caches: for example proxies and CDNs.
sMaxAge :: (MonadState (MFlowState v) m, MonadIO m) => Int -> m ()
sMaxAge secs =  set (SMaxAge secs)

-- | add @noTransform@ to the @Cache-Control@ header directive.
--
-- Tell CDNs that the content should not be transformed to save space and so on
noTransform :: (MonadState (MFlowState v) m, MonadIO m) => m ()
noTransform =  set NoTransform

-- | add @mustRevalidate@ to the @Cache-Control@ header directive.
--
-- the cache must verify that the page has not changed.
mustRevalidate  :: (MonadState (MFlowState v) m, MonadIO m) => m ()
mustRevalidate =  set MustRevalidate

-- | add @proxyRevalidate@ to the @Cache-Control@ header directive.
--
-- The same than mustRevalidate, for shared caches (proxies etc)
proxyRevalidate :: (MonadState (MFlowState v) m, MonadIO m) => m ()
proxyRevalidate =  set ProxyRevalidate

-- | add @etag <string>@ to the  header directives.
--
-- it is a resource identifier for the page that substitutes the URL identifier
etag :: (MonadState (MFlowState v) m, MonadIO m) =>  ByteString ->  m ()
etag s =  set (ETag s)

-- | add @vary <string>@ to the header directives.
--
-- Usually the page add this identifier to the URL string, that is the default identifier
-- So the same page with different etags will be cached and server separately
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



compile rs = comp $ Data.List.sort rs
   where
   comp []= []
   comp [x]= [x]
   comp (x:(xs@(x':_))) | x==x'= comp xs             -- !> ("drop repetitions "++ show x)
   comp (Private:Public: xs) = comp  $ Private:comp xs
   comp (NoCache:NoStore:xs)= comp $ NoCache: comp xs
   comp (NoStore: Expires _: xs)= comp $ NoStore: comp xs
   comp (NoStore:MaxAge _ : xs)= comp $ NoStore: comp xs
   comp (NoCache:MaxAge _ : xs)= comp $ NoCache: comp xs
   comp (SMaxAge t:SMaxAge t':xs)= comp $ MaxAge (Prelude.min t t'): comp xs
   comp (Expires t:Expires t':xs)= comp $ Expires t: comp xs
   comp (Expires t:MaxAge _:xs)= comp $ Expires t: comp xs
   comp (MaxAge t:MaxAge t':xs)= comp $ MaxAge (Prelude.min t t'): comp xs
   comp (x:xs) = x: comp xs

onNothing  mmx mmy= do
  mx <- mmx
  case mx of
   Just x -> return x
   Nothing -> mmy


-- | return the composition of the current directives. Used by the page internally
setCachePolicy :: (MonadState (MFlowState v) m, Monad m) => m ()
setCachePolicy= do
   rs <- getSessionData `onNothing` return  []
   let hs =generate $ compile rs                     -- !> show rs
   mapM_ (\(n,v) -> setHttpHeader n v ) hs           -- !> ("headers1="++ show hs)
