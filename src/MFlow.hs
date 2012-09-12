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

"MFlow.Wai" is a instantiation for the WAI interface.

In order to manage resources, there are primitives that kill the process and its state after a timeout.

All these details are hidden in the monad of "MFlow.Forms" that provides
an higuer level interface.

Fragment based streaming 'sendFragment' 'sendEndFragment' are  provided only at this level.

'stateless' and 'transient' serving processes are possible. `stateless` are request-response
 with no intermediate messaging dialog. `transient` processes have no persistent
 state, so they restart anew after a timeout or a crash.

-}


{-# LANGUAGE  DeriveDataTypeable, UndecidableInstances
              ,ExistentialQuantification, MultiParamTypeClasses
              ,FunctionalDependencies
              ,TypeSynonymInstances
              ,FlexibleInstances
              ,FlexibleContexts
              ,RecordWildCards
              ,OverloadedStrings
               #-}  
module MFlow (
Params,  Workflow, HttpData(..),Processable(..), ToHttpData(..)
, Token(..), ProcList
,flushRec, receive, receiveReq, receiveReqTimeout, send, sendFlush, sendFragment
, sendEndFragment
,msgScheduler, addMessageFlows,getMessageFlows, transient, stateless,anonymous
,noScript,hlog,addTokenToList,deleteTokenInList,
-- * ByteString tags
-- | basic but efficient tag formatting
btag, bhtml, bbody,Attribs)

where
import Control.Concurrent.MVar 
import Data.IORef
import GHC.Conc(unsafeIOToSTM)
import Data.Typeable
import Data.Maybe(isJust, isNothing, fromMaybe, fromJust)
import Data.Char(isSeparator)
import Data.List(isPrefixOf, elem , span, (\\))
import Control.Monad(when)

import Data.Monoid
import Control.Concurrent(forkIO,threadDelay,killThread, myThreadId, ThreadId)


import Unsafe.Coerce
import System.IO.Unsafe
import Data.TCache.DefaultPersistence  hiding(Indexable(..))

import Data.ByteString.Lazy.Char8 as B(ByteString, pack, unpack,empty,append,cons,fromChunks)

import qualified Data.Map as M
import System.IO
import System.Time
import Control.Workflow
import MFlow.Cookies

--import Debug.Trace
--(!>)= flip trace


--type Header= (String,String)
data HttpData = HttpData Params [Cookie] ByteString | Error WFErrors ByteString deriving (Typeable, Show)

instance ToHttpData HttpData where
 toHttpData= id

instance ToHttpData ByteString where
 toHttpData bs= HttpData [] [] bs

instance Monoid HttpData where
 mempty= HttpData [] [] empty
 mappend (HttpData h c s) (HttpData h' c' s')= HttpData (h++h') (c++ c') $ mappend s s'

-- | List of (wfname, workflow) pairs, to be scheduled depending on the message's pwfname
type ProcList = WorkflowList IO Token ()


data Req  = forall a.( Processable a,Typeable a)=> Req a   deriving Typeable

type Params =  [(String,String)]

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

data Resp  = Fragm HttpData
           | EndFragm HttpData
           | Resp HttpData

-- | The anonymous user
anonymous= "anon#"

-- | It is the path of the root flow
noScript = "noscript"

-- | a Token identifies a flow that handle messages. The scheduler compose a Token with every `Processable`
-- message that arrives and send the mesage to the appropriate flow.
data Token = Token{twfname,tuser, tind :: String , q :: MVar Req, qr :: MVar Resp}  deriving  Typeable

instance Indexable  Token  where
     key (Token w u i  _ _  )=
          if u== anonymous then  u++ i   -- ++ "@" ++ w
                           else  u       -- ++ "@" ++ w

instance Show Token where
     show t = "Token " ++ key t

instance Read Token where
     readsPrec _ ('T':'o':'k':'e': 'n':' ':str1)
       | anonymous `isPrefixOf` str1= [(Token  w anonymous i  (newVar 0) (newVar 0), tail str2)]
       | otherwise                 = [(Token  w ui "0"  (newVar 0) (newVar 0), tail str2)]

        where

        (ui,str')= span(/='@') str1
        i        = drop (length anonymous) ui
        (w,str2) = span (not . isSeparator) $ tail str'
        newVar _= unsafePerformIO  $ newEmptyMVar


     readsPrec _ str= error $ "parse error in Token read from: "++ str

instance Serializable Token  where
  serialize  = pack . show
  deserialize= read . unpack

iorefqmap= unsafePerformIO  . newMVar $ M.empty

addTokenToList t@Token{..} =
   modifyMVar_ iorefqmap $ \ map ->
     return $ M.insert  ( tind  ++ twfname  ++ tuser ) t map

deleteTokenInList t@Token{..} =
   modifyMVar_ iorefqmap $ \ map ->
     return $ M.delete  (tind  ++ twfname  ++ tuser) map

getToken msg=  do
      qmap  <- readMVar iorefqmap
      let u= puser msg ; w= pwfname msg ; i=pind msg
      let mqs = M.lookup ( i  ++ w  ++ u) qmap
      case mqs of
              Nothing  -> do
                 q <-   newEmptyMVar  -- `debug` (i++w++u)
                 qr <-  newEmptyMVar
                 let token= Token w u i  q qr
                 addTokenToList token
                 return token

              Just token-> return token


{-
instance  (Monad m, Show a) => Traceable (Workflow m a) where
       debugf iox str = do
              x <- iox
              return $ debug x (str++" => Workflow "++ show x)
-}
-- | send a complete response 
send ::  ToHttpData a => Token  -> a -> IO()
send  (Token _ _ _ queue qresp) msg=   do
       putMVar qresp  . Resp $ toHttpData msg

sendFlush t msg= flushRec t >> send t msg     -- !> "sendFlush "

-- | send a response fragment. Useful for streaming. the last packet must sent trough 'send'
sendFragment ::  ToHttpData a => Token  -> a -> IO()
sendFragment (Token _ _ _ _ qresp) msg=   putMVar qresp  . Fragm $ toHttpData msg

sendEndFragment :: ToHttpData a =>  Token  -> a -> IO()
sendEndFragment (Token _ _ _ _ qresp  ) msg=  putMVar qresp  . EndFragm  $ toHttpData msg

--emptyReceive (Token  queue _  _)= emptyQueue queue
receive ::  Typeable a => Token -> IO a
receive t= receiveReq t >>= return  . fromReq

flushRec t@(Token _ _ _ queue _)= do
   empty <-  isEmptyMVar  queue
   when (not empty) $ takeMVar queue >> return ()


receiveReq ::  Token -> IO Req
receiveReq (Token _ _ _ queue _)=   readMVar queue     -- !> "receiveReqSTM"

fromReq :: Typeable a => Req -> a
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
  let id= keyWF (twfname t)  t in withKillTimeout id time time2 (receiveReq t)


delMsgHistory t = do

      let statKey=  keyWF (twfname t)  t                  -- !> "wf"      --let qnme= keyWF wfname t
      delWFHistory1 statKey                                 -- `debug` "delWFHistory"
      


-- | executes a simple monadic computation that receive the params and return a response
--
-- It is used with `addMessageFlows` `hackMessageFlow` or `waiMessageFlow`
stateless ::  (ToHttpData b) => (Params -> IO b) -> (Token -> Workflow IO ())
stateless f = transient $ \tk ->do
    req <- receiveReq tk
    resp <- f (getParams req)
    sendFlush tk resp

-- | Executes a monadic computation that send and receive messages, but does
-- not store its state in permanent storage. The process once stopped, will restart anew 
--
---- It is used with `addMessageFlows` `hackMessageFlow` or `waiMessageFlow`
transient :: (Token -> IO ()) -> (Token -> Workflow IO ())  
transient f=  unsafeIOtoWF . f -- WF(\s -> f t>>= \x-> return (s, x) )


_messageFlows :: MVar (M.Map String (Token-> Workflow IO ()))
_messageFlows= unsafePerformIO $ newMVar M.empty -- [(String,Token  -> Workflow IO ())])

-- | add a list of flows to be scheduled. Each entry in the list is a pair @(path, flow)@
addMessageFlows wfs=  modifyMVar_ _messageFlows(\ms ->  return $ M.union ms  (M.fromList wfs))

-- | return the list of the scheduler
getMessageFlows = readMVar _messageFlows

class ToHttpData a  where
    toHttpData :: a -> HttpData 



--tellToWF :: (Typeable a,  Typeable c, Processable a) => Token -> a -> IO c
tellToWF (Token _ _ _ queue qresp ) msg = do  
    putMVar queue $ Req msg    
    m <-  takeMVar qresp  -- !> ("********antes de recibir" ++ show(unsafePerformIO myThreadId))
    case m  of
        Resp r  ->  return  r  -- !> ("*********** RECIBIDO"++ show(unsafePerformIO myThreadId))
        Fragm r -> do
                   result <- getStream   r
                   return  result

                    
    where

    getStream r =  do
         mr <-  takeMVar qresp 
         case mr of
            Resp _ -> error "\"send\" used instead of \"sendFragment\" or \"sendEndFragment\""
            Fragm h -> do
                 rest <- unsafeInterleaveIO $  getStream  h
                 let result=  mappend  r   rest
                 return  result 
            EndFragm h -> do
                 let result=  mappend r   h
                 return  result






--data Error= Error String deriving (Read, Show, Typeable)

instance ToHttpData String where
  toHttpData= HttpData [] [] . pack

-- | The scheduler creates a Token with every `Processable`
-- message that arrives and send the mesage to the appropriate flow, get the response
-- and return it.
msgScheduler
  :: (Typeable a,Processable a)
  => a  -> IO (HttpData, ThreadId)
msgScheduler x  = do
  token <- getToken x

  th <- startMessageFlow (pwfname x) token
  r<- tellToWF token  x
  return (r,th)
  where
  
  startMessageFlow wfname token = 
   forkIO $ do
        wfs <- getMessageFlows
        r <- startWF wfname  token   wfs                      -- !>( "init wf " ++ wfname)
        case r of
          Left NotFound -> sendFlush token (Error NotFound $ "Not found: " <> pack wfname)
          Left AlreadyRunning -> return ()                    -- !> ("already Running " ++ wfname)
          Left Timeout -> return()                            -- !>  "Timeout in msgScheduler"
          Left (WFException e)-> do
               let user= key token
               print e
               logError user wfname e
               moveState wfname token token{tuser= "error/"++tuser token}

               case user of
                 "admin" -> sendFlush token (show e)           -- !> ("WF error: "++ show e)
                 _       -> sendFlush token ("An Error has ocurred." :: ByteString)

          Right _ -> do
--               let msg= "finished Flow "++ wfname++ " restarting"
--               logError (key token) wfname msg
--               startMessageFlow wfname token wfs

              delMsgHistory token; return ()      -- !> ("finished " ++ wfname)


  logError u wf e= do
     hSeek hlog SeekFromEnd 0
     TOD t _ <- getClockTime
     hPutStrLn hlog (","++show [u,show t,wf,e])  >> hFlush hlog

logFileName= "errlog"

-- | The handler of the error log
hlog= unsafePerformIO $ openFile logFileName ReadWriteMode


-- basic bytestring XML tags
type Attribs= [(String,String)]
-- | Writes a XML tag in a ByteString. It is the most basic form of formatting. For
-- more sophisticated formatting , use "MFlow.Forms.XHtml" or "MFlow.Forms.HSP".
btag :: String -> Attribs  -> ByteString -> ByteString
btag t rs v= "<" `append` pt `append` attrs rs `append` ">" `append` v `append`"</"`append` pt `append` ">"
 where
 pt= pack t
 attrs []= B.empty
 attrs rs=  pack $ concatMap(\(n,v) -> (' ' :   n) ++ "=" ++  v ) rs
-- |
-- > bhtml ats v= btag "html" ats v
bhtml :: Attribs -> ByteString -> ByteString
bhtml ats v= btag "html" ats v

-- |
-- > bbody ats v= btag "body" ats v
bbody :: Attribs -> ByteString -> ByteString
bbody ats v= btag "body" ats v
