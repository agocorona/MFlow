{-# LANGUAGE  UndecidableInstances
             , TypeSynonymInstances
             , MultiParamTypeClasses
             , DeriveDataTypeable
             , FlexibleInstances
             , OverloadedStrings #-}
             
module MFlow.Wai(
     module MFlow.Cookies
    ,module MFlow
    ,waiMessageFlow)
where

import Data.Typeable
import Network.Wai

import Control.Concurrent.MVar(modifyMVar_, readMVar)
import Control.Monad(when)


import qualified Data.ByteString.Lazy.Char8 as B(empty,pack, unpack, length, ByteString,tail)
import Data.ByteString.Lazy(fromChunks)
import qualified Data.ByteString.Char8 as SB
import Control.Concurrent(ThreadId(..))
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Trans
import Control.Exception
import qualified Data.Map as M
import Data.Maybe 
import Data.TCache
import Data.TCache.DefaultPersistence
import Control.Workflow hiding (Indexable(..))

import MFlow
import MFlow.Cookies
import Data.Monoid
import MFlow.Wai.Response
import Network.Wai
import Network.HTTP.Types hiding (urlDecode)
import Data.Conduit
import Data.Conduit.Lazy
import qualified Data.Conduit.List as CList
import Data.CaseInsensitive
import System.Time

--import Debug.Trace
--(!>)= flip trace

flow=  "flow"


instance Processable Request  where
   pwfname  env=  if SB.null sc then noScript else SB.unpack sc
      where
      sc=  SB.tail $ rawPathInfo env
   
   puser env = fromMaybe anonymous $ fmap SB.unpack $ lookup ( mk $SB.pack cookieuser) $ requestHeaders env
                    
   pind env= fromMaybe (error ": No FlowID") $ fmap SB.unpack $ lookup  (mk $ SB.pack flow) $ requestHeaders env
   getParams=    mkParams1 . requestHeaders
     where
     mkParams1 = Prelude.map mkParam1
     mkParam1 ( x,y)= (SB.unpack $ original  x, SB.unpack y)

--   getServer env= serverName env
--   getPath env= pathInfo env
--   getPort env= serverPort env
   




splitPath ""= ("","","")
splitPath str=
       let
            strr= reverse str
            (ext, rest)= span (/= '.') strr
            (mod, path)= span(/='/') $ tail rest
       in   (tail $ reverse path, reverse mod, reverse ext)


waiMessageFlow  ::  Request ->  ResourceT IO Response
waiMessageFlow req1=   do
     let httpreq1= getParams  req1 

     let cookies=getCookies  httpreq1

     (flowval , retcookies) <-  case lookup flow cookies of
              Just fl -> return  (fl, [])
              Nothing  -> do
                     fl <- liftIO $ newFlow
                     return (fl,  [(flow,  fl, "/",Nothing)::Cookie])
                     
{-  for state persistence in cookies 
     putStateCookie req1 cookies
     let retcookies= case getStateCookie req1 of
                                Nothing -> retcookies1
                                Just ck -> ck:retcookies1
-}

     input <- case   parseMethod $ requestMethod req1  of
              Right POST -> if  lookup  ("Content-Type") httpreq1 == Just "application/x-www-form-urlencoded"
                  then do
                   inp <- liftIO $ runResourceT (requestBody req1 $$ CList.consume)
                   return .  urlDecode $ concatMap SB.unpack  inp
                  else return []
                  
              Right GET -> let tail1 s | s==SB.empty =s
                               tail1 xs= SB.tail xs
                           in return . urlDecode $  SB.unpack   . tail1 $ rawQueryString req1  --  !> (SB.unpack $ rawQueryString req1)
              x ->  return [] 
     let req = case retcookies of
          [] -> req1{requestHeaders=  mkParams (input ++ cookies) ++ requestHeaders req1}  -- !> "REQ"
          _  -> req1{requestHeaders=  mkParams ((flow, flowval): input ++ cookies) ++ requestHeaders req1}  --  !> "REQ"


     (resp',th) <- liftIO $ msgScheduler req  -- !> (show $ requestHeaders req)

     let resp= case (resp',retcookies) of
            (_,[]) -> resp'
            (error@(Error _ _),_) -> error
            (HttpData hs co str,_) -> HttpData hs (co++ retcookies)  str

     return $ toResponse resp


------persistent state in cookies (not tested)

tvresources ::  MVar (Maybe ( M.Map string string))
tvresources= unsafePerformIO $ newMVar  Nothing
statCookieName= "stat"

putStateCookie req cookies=
    case lookup statCookieName cookies of
        Nothing -> return ()
        Just (statCookieName,  str , "/", _) -> modifyMVar_ tvresources $
                          \mmap -> case mmap of
                              Just map ->  return $ Just $ M.insert (keyResource req)  str map
                              Nothing   -> return $ Just $ M.fromList [((keyResource req),  str) ]

getStateCookie req= do
    mr<- readMVar tvresources
    case mr of
     Nothing  ->  return Nothing
     Just map -> case  M.lookup (keyResource req) map  of
      Nothing -> return Nothing
      Just str -> do
        swapMVar tvresources Nothing
        return $  Just  (statCookieName,  str , "/")

{-
persistInCookies= setPersist  PersistStat{readStat=readResource, writeStat=writeResource, deleteStat=deleteResource}
    where
    writeResource stat= modifyMVar_ tvresources $  \mmap -> 
                                      case mmap of
                                            Just map-> return $ Just $ M.insert (keyResource stat) (serialize stat) map
                                            Nothing -> return $ Just $ M.fromList [((keyResource stat),   (serialize stat)) ]
    readResource stat= do
           mstr <- withMVar tvresources $ \mmap -> 
                                case mmap of
                                   Just map -> return $ M.lookup (keyResource stat) map
                                   Nothing -> return  Nothing
           case mstr of
             Nothing -> return Nothing
             Just str -> return $ deserialize str

    deleteResource stat= modifyMVar_ tvresources $  \mmap-> 
                              case mmap of
                                  Just map -> return $ Just $ M.delete  (keyResource stat) map
                                  Nothing ->  return $ Nothing

-}
