{-# LANGUAGE  UndecidableInstances
             , CPP
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
import Data.ByteString.UTF8  hiding (span)
import qualified Data.ByteString.Char8 as SB -- hiding (pack, unpack)
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
import Network.Wai.Parse
import qualified  Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import Network.HTTP.Types 
import Data.Conduit
import Data.Conduit.Lazy
import qualified Data.Conduit.List as CList
import Data.CaseInsensitive
import System.Time
import System.Directory
import System.IO
import qualified Data.Text as T


import Debug.Trace
(!>) = flip trace

flow=  "flow"

instance Processable Request  where
   pwfPath  env=  if Prelude.null sc then [noScript] else Prelude.map T.unpack sc
      where
      sc= let p= pathInfo env
              p'= reverse p
          in case p' of
            [] -> []
            p' -> if T.null $ head p' then  reverse(tail  p') else p


   puser env = fromMaybe anonymous $ fmap toString $ lookup ( mk $ fromString cookieuser) $ requestHeaders env

   pind env= fromMaybe (error ": No FlowID") $ fmap toString $ lookup  (mk flow) $ requestHeaders env
   getParams=    mkParams1 . requestHeaders
     where
     mkParams1 = Prelude.map mkParam1
     mkParam1 ( x,y)= (toString $ original  x, toString y)

toApp :: (Request -> IO Response) -> Application
#if MIN_VERSION_wai(3, 0, 0)
toApp f req sendResponse = f req >>= sendResponse
#else
toApp = id
#endif

waiMessageFlow  ::  Application
waiMessageFlow = toApp $ \req1 -> do
     let httpreq1= requestHeaders  req1

     let cookies = getCookies  httpreq1

     (flowval , retcookies) <-  case lookup flow cookies of
              Just fl -> return  (fl, [])
              Nothing  -> do
                     fl <- liftIO $ newFlow
                     return (fl,  [UnEncryptedCookie (flow,  fl, "/",Nothing):: Cookie])

{-   for state persistence in cookies
     putStateCookie req1 cookies
     let retcookies= case getStateCookie req1 of
                                Nothing -> retcookies1
                                Just ck -> ck:retcookies1
-}

     (params,files) <- case parseMethod $ requestMethod req1  of
              Right GET -> do
                   return (Prelude.map (\(x,y) -> (x,fromMaybe "" y)) $ queryString req1,[])

              Right POST -> do

                 case getRequestBodyType req1  of
                     Nothing -> error $ "getRequestBodyType: "
                     Just rbt ->
                         runResourceT $ withInternalState $ \state -> liftIO $ do
                               let backend file info= do
                                    (key, (fp, h)) <- flip runInternalState state $ allocate (do
                                        tempDir <- getTemporaryDirectory
                                        openBinaryTempFile tempDir "upload.tmp") (\(_, h) -> hClose h)
                                    CB.sinkHandle h
                                    lift $ release key
                                    return fp
#if MIN_VERSION_wai(3, 0, 0)
                               let backend' file info getBS = do
                                        let src = do
                                                bs <- liftIO getBS
                                                when (not $ SB.null bs) $ do
                                                    Data.Conduit.yield bs
                                                    src
                                        src $$ backend file info
                               sinkRequestBody backend' rbt (requestBody req1)
#else
                               requestBody req1 $$ sinkRequestBody backend rbt
#endif

--                         let fileparams= Prelude.map (\(param,FileInfo filename contentype content)
--                                              -> (param,   SB.pack content )) files
--                         let fileparams= Prelude.map (\(param,fileinfo)
--                                              -> (param,  fileinfo )) files
--                         return $ fileparams++ params
     let filesp= Prelude.map (\(param,FileInfo filename contentype tempfile)
                                              -> (mk param,  fromString $ show(filename,contentype,tempfile) )) files
--     let filesp= Prelude.map (\(a,b) -> ( mk a, fromString $ show b)) files


     let req = case retcookies of
          [] -> req1{requestHeaders= filesp ++ mkParams (params  ++ cookies) ++ requestHeaders req1}  
          _  -> req1{requestHeaders= filesp ++ mkParams ((flow, flowval): params ++ cookies) ++ requestHeaders req1}


     (resp',th) <- liftIO $ msgScheduler req      -- !> (show $ requestHeaders req)

     let resp= case (resp',retcookies) of
            (_,[]) -> resp'
            (error@(Error _),_) -> error
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
