{- | Non monadic low level support stuff for the MFlow application server.
it implements an scheduler of queued 'Processable'  messages that are served according with
the source identification and the verb invoked.
Ths scheduler executed the appropriate workflow (using the workflow package)
the workflow may send additional messages to the source, identified by a 'Token'
. The computation state is logged and can be recovered.

The message communication is trough  polimorphic, monoidal queues.
There is no asumption about message codification, so instantiations
of this scheduler for many different infrastructures is possible.
"MFlow.Hack" is an instantiation for the Hack interface in a Web context.

In order to manage resources, the serving process may die after a timeout.
as well as the logged state, usually, after a longer timeout .

All these details are hidden in the monad of "MFlow.Forms" that provides
an higuer level interface. Altroug fragments streaming 'sendFragment' 'sendEndFragment'
are only provided at this level.

'stateless' and 'transient' serving processes are possible. `stateless` are request-response
 with no intermediate messaging dialog. `transient` processes have no persistent
 state, so they restart anew after a timeout or a crash.

-}


{-# LANGUAGE  DeriveDataTypeable, UndecidableInstances
              ,ExistentialQuantification, MultiParamTypeClasses
              ,FunctionalDependencies
              ,TypeSynonymInstances
              ,FlexibleInstances
              ,FlexibleContexts #-}  
module MFlow (
Params, Req(..), Resp(..), Workflow, HttpData(..),Processable(..), ConvertTo(..), Token(..), getToken, Error(..), ProcList
,flushRec, receive, receiveReq, receiveReqTimeout, send, sendFlush, sendFragment, sendEndFragment
,msgScheduler, addMessageFlows,getMessageFlows, transient, stateless,anonymous
,noScript,logFileName)

where
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import GHC.Conc(unsafeIOToSTM)
import Data.Typeable
import Data.Maybe(isJust, isNothing, fromMaybe, fromJust)
import Data.Char(isSeparator)
import Data.List(isPrefixOf, elem , span, (\\))
import Control.Monad(when)

import Data.Monoid
import Control.Concurrent(forkIO,threadDelay,killThread, myThreadId, ThreadId)
import Control.Concurrent.MVar

import Unsafe.Coerce
import System.IO.Unsafe
import Data.TCache.DefaultPersistence

import Data.ByteString.Lazy.Char8(pack, unpack)

import qualified Data.Map as M
import System.IO
import Control.Workflow

import MFlow.Cookies

import Debug.Trace

(!>)= flip trace

data HttpData a= HttpData [Cookie] a deriving Typeable

-- | List of (wfname, workflow) pairs, to be scheduled depending on the message's pwfname
type ProcList = WorkflowList IO Token ()


data Req  = forall a.( Processable a,Typeable a)=> Req a   deriving Typeable

type Params = [(String,String)]

class  Processable a where
     pwfname :: a -> String
     puser :: a -> String
     pind :: a -> String
     getParams :: a -> Params
--     getServer ::a -> String
--     getPath :: a -> String
--     getPort :: a -> Int


instance Processable  Req   where
    pwfname (Req x)= pwfname x
    puser (Req x)= puser x
    pind (Req x)= pind x   
    getParams (Req x)= getParams  x
--    getServer (Req x)= getServer  x
--    getPath (Req x)= getPath  x
--    getPort (Req x)= getPort  x

data Resp  = forall  a c.( Typeable a,Typeable c, Monoid c, ConvertTo a c)=> Fragm a
            | forall a c.( Typeable a,Typeable c,  Monoid c,  ConvertTo a c)=> EndFragm a
            | forall a c.(  Typeable a,Typeable c, ConvertTo a c) => Resp a


anonymous= "anon#"
noScript = "noscript"

data Token = Token{twfname,tuser, tind :: String , q :: TChan Req, qr :: TChan Resp}  deriving  Typeable

instance Indexable  Token  where
     key (Token w u i _ _  )=
          if u== anonymous then  u++ i   -- ++ "@" ++ w
                           else  u       -- ++ "@" ++ w

instance Show Token where
     show t = "Token " ++ key t

instance Read Token where
     readsPrec _ ('T':'o':'k':'e': 'n':' ':str1)
       | anonymous `isPrefixOf` str1= [(Token  w anonymous i (newChan 0) (newChan 0), tail str2)]
       | otherwise                 = [(Token  w ui "0" (newChan 0) (newChan 0), tail str2)]

        where

        (ui,str')= span(/='@') str1
        i        = drop (length anonymous) ui
        (w,str2) = span (not . isSeparator) $ tail str'
        newChan _= unsafePerformIO  newTChanIO

     readsPrec _ str= error $ "parse error in Token read from: "++ str

instance Serializable Token  where
  serialize  = pack . show
  deserialize= read . unpack

iorefqmap= unsafePerformIO  . newMVar $ M.empty

getToken msg=  do
      qmap  <- readMVar iorefqmap
      let u= puser msg ; w= pwfname msg ; i=pind msg
      let mqs = M.lookup ( i  ++ w  ++ u) qmap
      (q,qr) <- case mqs of
              Nothing  -> do
                 q <-  atomically $ newTChan  -- `debug` (i++w++u)
                 qr <- atomically $ newTChan
                 let qs= (q,qr)
                 modifyMVar_ iorefqmap $ \ map -> return $ M.insert  ( i  ++ w  ++ u) qs map
                 return qs

              Just qs -> return qs
             
      return (Token w u i q qr )   --`debug1` "returning getToken"
{-
instance  (Monad m, Show a) => Traceable (Workflow m a) where
       debugf iox str = do
              x <- iox
              return $ debug x (str++" => Workflow "++ show x)
-}
-- | send a complete response 
send ::  (Typeable a, Typeable b, ConvertTo a b) => Token  -> a -> IO()
send  (Token _ _ _ queue qresp) msg= atomically $ do
       writeTChan qresp  $ Resp msg

sendFlush t msg= flushRec t >> send t msg

-- | send a response fragment. Useful for streaming. the last packet must sent trough 'send'
sendFragment ::  ( Typeable a, Typeable b, Monoid b, ConvertTo a b) => Token  -> a -> IO()
sendFragment (Token _ _ _ _ qresp) msg=  atomically $ writeTChan qresp  $ Fragm msg

sendEndFragment ::  ( Typeable a, Typeable b, Monoid b, ConvertTo a b) => Token  -> a -> IO()
sendEndFragment (Token _ _ _ _ qresp  ) msg=  atomically $ writeTChan qresp  $ EndFragm msg

--emptyReceive (Token  queue _  _)= emptyQueue queue
receive :: (Processable a, Typeable a) => Token -> IO a
receive t= receiveReq t >>= return  . fromReq

flushRec t@(Token _ _ _ queue _)=   do
  empty <- atomically $ isEmptyTChan  queue
  if empty then  return() else atomically(readTChan queue) >> flushRec t

receiveReq ::  Token -> IO Req
receiveReq = atomically . receiveReqSTM

receiveReqSTM :: Token -> STM Req
receiveReqSTM (Token _ _ _ queue _)=   readTChan queue

fromReq :: (Processable a, Typeable a) => Req -> a
fromReq  (Req x) = x' where
      x'= case cast x of
           Nothing -> error $ "receive: received type: "++ show (typeOf x) ++ " does not match the desired type:" ++ show (typeOf  x')
           Just y  -> y



receiveReqTimeout :: Int
                   -> Integer
                   -> Token
                   -> IO Req
receiveReqTimeout 0 0 t= receiveReq t
receiveReqTimeout time time2 t=
  let id= twfname t ++ "#" ++key t in withKillTimeout id time time2 (receiveReqSTM t)


delMsgHistory t = do
      let qnme=key t
      let statKey= twfname t ++ "#" ++ qnme                 -- !> "wf"      --let qnme= keyWF wfname t
      delWFHistory1 statKey                                 -- `debug` "delWFHistory"
      


-- ! to add a simple monadic computation of type (a -> IO b)  to the list
-- of the scheduler
stateless :: ( Typeable a, Processable a, Typeable b
             , ConvertTo b c, Typeable c)
           =>  (a -> IO b) -> (Token -> Workflow IO ())
stateless f = transient $ \tk -> receive tk >>= f >>= send tk

-- | to add a monadic computation that send and receive messages, but does
-- not store its state in permanent storage.
transient :: (Token -> IO ()) -> (Token -> Workflow IO ())  
transient f=  unsafeIOtoWF . f -- WF(\s -> f t>>= \x-> return (s, x) )


_messageFlows :: MVar ProcList
_messageFlows= unsafePerformIO $ newMVar [] -- [(String,Token  -> Workflow IO ())])


addMessageFlows wfs=  modifyMVar_ _messageFlows(\ms ->  return $ ms ++ wfs)

getMessageFlows = readMVar _messageFlows

class ConvertTo a b |  a -> b where
    convert :: a -> b



--tellToWF :: (Typeable a,  Typeable c, Processable a) => Token -> a -> IO c
tellToWF (Token _ _ _ queue qresp ) msg = do
    atomically $ writeTChan queue $ Req msg                              -- `debug`  ("tell wf="++wf)
    m <- atomically $ readTChan qresp
    case m  of
        Resp r  ->  return . cast1 $ convert r
        Fragm r -> do
                   result <- getStream  $ convert r
                   return $ cast1 result

                    
    where


    cast1 :: (Typeable a, Typeable b) => a -> b
    cast1 x= y where
       y= case cast x of
            Nothing ->  error $ "cast error: " ++  show (typeOf x) ++ " to " ++ show (typeOf y)
            Just y -> y
    getStream :: (Typeable a, Monoid a) => a -> IO  a
    getStream r =  do
         mr <- atomically $ readTChan qresp 
         case mr of
            Resp _ -> error "\"send\" used instead of \"sendFragment\" or \"sendEndFragment\""
            Fragm h -> do
                 rest <- unsafeInterleaveIO $  getStream ( convert h)
                 let result=  mappend  r  (cast1 rest)
                 return (cast1 result )
            EndFragm h -> do
                 let result=  mappend r  $ cast1 (convert h)
                 return  (cast1 result)






data Error= Error String deriving (Read, Show, Typeable)

instance Indexable Error where
    key _= error "Idexablew instance for Error"

msgScheduler
  :: (Processable a, Typeable a, ConvertTo  Error c, Typeable c)
  => a -> ProcList -> IO (c, ThreadId)
msgScheduler x wfs = do
  token <- getToken x

  th <- startMessageFlow (pwfname x) token  wfs
  r<- tellToWF token  x
  return (r,th)
  where
  
  startMessageFlow wfname token wfs= 
   forkIO $ do
        r <- startWF wfname  token   wfs                     -- !>( "init wf " ++ wfname)
        case r of
          Left NotFound -> error ( "Not found: "++ wfname)
          Left AlreadyRunning -> return ()                  -- !> ("already Running " ++ wfname)
          Left Timeout -> return()                          -- !>  "Timeout in msgScheduler"
          Left (Exception e)-> do
               let user= key token
               logError user e
               case user of
                 "admin" -> send token $ Error (show e)     -- !> ("WF error: "++ show e)
                 _       -> send token $ Error "An Error has ocurred"

               moveState wfname token token{tuser= "error/"++tuser token}

          Right _ -> do  delMsgHistory token; return ()   -- !> ("finished " ++ wfname)
  logError u e= hPutStrLn hlog (show (u,e))  >> hFlush hlog

logFileName= "errlog"
hlog= unsafePerformIO $ openFile logFileName WriteMode





