{-# LANGUAGE  UndecidableInstances
             , TypeSynonymInstances
             , MultiParamTypeClasses
             , DeriveDataTypeable
             , FlexibleInstances #-}
             
module MFlow.Hack(
     module MFlow.Cookies
    ,module MFlow
    ,hackMessageFlow)
where

import Data.Typeable
import Hack

import Control.Concurrent.MVar(modifyMVar_, readMVar)
import Control.Monad(when)


import Data.ByteString.Lazy.Char8 as B(pack, unpack, length, ByteString)
import Control.Concurrent(ThreadId(..))
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import qualified Data.Map as M
import Data.Maybe 
import Data.TCache
import Data.TCache.DefaultPersistence
import Control.Workflow hiding (Indexable(..))

import MFlow
import MFlow.Cookies

import MFlow.Hack.Response
import Data.Monoid
import Data.CaseInsensitive
import System.Time

import System.Time


flow=  "flow"

instance Processable Env  where
   pwfname  env=  if null sc then noScript else sc
      where
      sc=  tail $ pathInfo env
   puser env = fromMaybe anonymous $ lookup  cookieuser $ http env
                    
   pind env= fromMaybe (error ": No FlowID") $ lookup flow $ http env
   getParams=  http
--   getServer env= serverName env
--   getPath env= pathInfo env
--   getPort env= serverPort env

   
data Flow= Flow !Integer deriving (Read, Show, Typeable)

instance Serializable Flow where
  serialize= B.pack . show
  deserialize= read . B.unpack

instance Indexable Flow where
  key _= "Flow"


rflow= getDBRef . key $ Flow undefined

newFlow= do
        TOD t _ <- getClockTime
        atomically $ do 
                    Flow n <- readDBRef rflow `onNothing` return (Flow 0)
                    writeDBRef rflow . Flow $ n+1
                    return . show $ t + n
         

                    
---------------------------------------------



--
--instance ConvertTo String TResp  where
--      convert = TResp . pack
--
--instance ConvertTo ByteString TResp  where
--      convert = TResp
--
--
--instance ConvertTo Error TResp where
--     convert (Error e)= TResp . pack  $ errorResponse e
--
--instance ToResponse v =>ConvertTo (HttpData v) TResp where
--    convert= TRespR


--webScheduler   :: Env
--               -> ProcList
--               -> IO (TResp, ThreadId)
--webScheduler = msgScheduler 

--theDir= unsafePerformIO getCurrentDirectory

wFMiddleware :: (Env -> Bool) -> (Env-> IO Response) ->   (Env -> IO Response)
wFMiddleware filter f = \ env ->  if filter env then hackMessageFlow env    else f env -- !> "new message"

-- | An instance of the abstract "MFlow" scheduler to the Hack interface
-- it accept the list of processes being scheduled and return a hack handler
--
-- Example:
--
-- @main= do
--
--   putStrLn $ options messageFlows
--   'run' 80 $ 'hackMessageFlow'  messageFlows
--   where
--   messageFlows=  [(\"main\",  'runFlow' flowname )
--                  ,(\"hello\", 'stateless' statelesproc)
--                  ,(\"trans\", 'transient' $ runflow transientflow]
--   options msgs= \"in the browser choose\\n\\n\" ++
--     concat [ "http:\/\/server\/"++ i ++ "\n" | (i,_) \<- msgs]
-- @
--hackMessageFlow :: [(String, (Token -> Workflow IO ()))]
--                -> (Env -> IO Response)
--hackMessageFlow  messageFlows = 
-- unsafePerformIO (addMessageFlows messageFlows) `seq`
-- hackWorkflow -- wFMiddleware f   other
-- where
-- f env = unsafePerformIO $ do
--    paths <- getMessageFlows >>=
--    return (pwfname env `elem` paths)

-- other= (\env -> defaultResponse $  "options: " ++ opts)
-- (paths,_)= unzip messageFlows
-- opts= concatMap  (\s -> "<a href=\""++ s ++"\">"++s ++"</a>, ") paths


splitPath ""= ("","","")
splitPath str=
       let
            strr= reverse str
            (ext, rest)= span (/= '.') strr
            (mod, path)= span(/='/') $ tail rest
       in   (tail $ reverse path, reverse mod, reverse ext)



hackMessageFlow  ::  Env ->  IO Response
hackMessageFlow req1=   do
     let httpreq1= http  req1  
     let cookies= {-# SCC "getCookies" #-} getCookies  httpreq1

     (flowval , retcookies) <-  case lookup ( flow) cookies of
              Just fl -> return  (fl, [])
              Nothing  -> do
                     fl <- newFlow
                     return ( fl,  [( flow,  fl,  "/",(Just $ show $ 365*24*60*60))])
                     
{-  for state persistence in cookies 
     putStateCookie req1 cookies
     let retcookies= case getStateCookie req1 of
                                Nothing -> retcookies1
                                Just ck -> ck:retcookies1
-}

     let input=
           case  ( requestMethod req1, lookup  "Content-Type" httpreq1 )  of
              (POST,Just "application/x-www-form-urlencoded") -> urlDecode . unpack $ hackInput  req1 
              (GET, _) -> urlDecode . queryString $ req1
              _ -> []

     let req = case retcookies of
          [] -> req1{http=  (input ++ cookies) ++ http req1}   -- !> "REQ"
          _  -> req1{http=(flow, flowval): ( input ++ cookies ) ++ http req1}   -- !> "REQ"


     (resp',th) <- msgScheduler  req


     let resp''= toResponse resp'
     let headers1= case retcookies of [] -> headers resp''; _ -> ctype :   (cookieHeaders retcookies)
     let resp =   resp''{status=200, headers= headers1 {-,("Content-Length",show $ B.length x) -}}

     return resp


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
