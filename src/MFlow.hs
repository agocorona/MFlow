{- | Non monadic low level primitives that implement the MFlow application server.
See "MFlow.Form" for the higher level interface that you may use.

it implements an scheduler of  'Processable'  messages that are served according with
the source identification and the verb invoked.
The scheduler executed the appropriate workflow (using the workflow package).
The workflow will send additional messages to the source and wait for the responses.
The diaglog is identified by a 'Token', which is associated to the flow.
. The computation state is optionally logged. On timeout, the process is killed. When invoked again,
the execution state is recovered as if no interruption took place.

There is no asumption about message codification, so instantiations
of this scheduler for different infrastructures is possible,
including non-Web based ones as long as they support or emulate cookies.

"MFlow.Hack" is an instantiation for the Hack interface in a Web context.

"MFlow.Wai" is a instantiation for the WAI interface.

"MFlow.Forms" implements a monadic type safe interface with composabe widgets and and applicative
combinator as well as an higher comunication interface.

"MFlow.Forms.XHtml" is an instantiation for the Text.XHtml format

"MFlow.Forms.Blaze.Html" is an instantaiation for  blaze-html

"MFlow.Forms.HSP"  is an instantiation for the Haskell Server Pages  format

There are some @*.All@ packages that contain a mix of these instantiations.
For exmaple, "MFlow.Wai.Blaze.Html.All" includes most of all necessary for using MFlow with
Wai <http://hackage.haskell.org/package/wai> and
Blaze-html <http://hackage.haskell.org/package/blaze-html>


In order to manage resources, there are primitives that kill the process and its state after a timeout.

All these details are hidden in the monad of "MFlow.Forms" that provides an higher level interface.

Fragment based streaming: 'sendFragment'  are  provided only at this level.

'stateless' and 'transient' server processeses are also possible. the first are request-response
 . `transient` processes do not persist after timeout, so they restart anew after a timeout or a crash.

-}


{-# LANGUAGE  DeriveDataTypeable, UndecidableInstances
              ,ExistentialQuantification
              ,MultiParamTypeClasses
              ,FunctionalDependencies
              ,TypeSynonymInstances
              ,FlexibleInstances
              ,FlexibleContexts
              ,RecordWildCards
              ,OverloadedStrings
              ,ScopedTypeVariables
              ,BangPatterns
               #-}  
module MFlow (
Flow, Params, HttpData(..),Processable(..)
, Token(..), ProcList
-- * low level comunication primitives. Use `ask` instead
,flushRec, flushResponse, receive, receiveReq, receiveReqTimeout, send, sendFlush, sendFragment
, sendEndFragment, sendToMF
-- * Flow configuration
,setNoScript,addMessageFlows,getMessageFlows,delMessageFlow, transient, stateless,anonymous
,noScript,hlog, setNotFoundResponse,getNotFoundResponse,
-- * ByteString tags
-- | very basic but efficient bytestring tag formatting
btag, bhtml, bbody,Attribs, addAttrs
-- * user
, userRegister, setAdminUser, getAdminName, Auth(..),getAuthMethod, setAuthMethod
-- * static files
-- * config
,config,getConfig
,setFilesPath
-- * internal use
,addTokenToList,deleteTokenInList, msgScheduler,serveFile,mimeTable, newFlow
,UserStr,PasswdStr, User(..),eUser

)
where
import Control.Concurrent.MVar 
import Data.IORef
import GHC.Conc(unsafeIOToSTM)
import Data.Typeable
import Data.Maybe(isJust, isNothing, fromMaybe, fromJust)
import Data.Char(isSeparator) 
import Data.List(isPrefixOf,isSuffixOf,isInfixOf, elem , span, (\\),intersperse)
import Control.Monad(when) 

import Data.Monoid
import Control.Concurrent(forkIO,threadDelay,killThread, myThreadId, ThreadId)
import Data.Char(toLower)

import Unsafe.Coerce
import System.IO.Unsafe
import Data.TCache
import Data.TCache.DefaultPersistence  hiding(Indexable(..))
import Data.TCache.Memoization
import qualified Data.ByteString.Lazy.Char8 as B  (head, readFile,ByteString, concat,pack, unpack,empty,append,cons,fromChunks)
import Data.ByteString.Lazy.Internal (ByteString(Chunk))
import qualified Data.ByteString.Char8 as SB
import qualified Data.Map as M
import System.IO
import System.Time
import Control.Workflow
import MFlow.Cookies
import Control.Monad.Trans
import qualified Control.Exception as CE
import Data.RefSerialize hiding (empty)
import qualified Data.Text as T
import System.Posix.Internals
import Control.Exception
import Crypto.PasswordStore


--import Debug.Trace
--(!>)  =   flip trace


-- | a Token identifies a flow that handle messages. The scheduler compose a Token with every `Processable`
-- message that arrives and send the mesage to the appropriate flow.
data Token = Token{twfname,tuser, tind :: String , tpath :: [String], tenv:: Params, tblock:: MVar Bool, tsendq :: MVar Req, trecq :: MVar Resp}  deriving  Typeable

instance Indexable  Token  where
     key (Token w u i _ _ _ _ _  )=  i
--          if u== anonymous then  u ++ i   -- ++ "@" ++ w
--                          else  u       -- ++ "@" ++ w

instance Show Token where
     show t = "Token " ++ key t

instance Read Token where
     readsPrec _ ('T':'o':'k':'e': 'n':' ':str1)
       | anonymous `isPrefixOf` str1= [(Token  w anonymous i [] [] (newVar True) (newVar 0) (newVar 0), tail str2)]
       | otherwise                 = [(Token  w ui "0" [] [] (newVar True)  (newVar 0) (newVar 0), tail str2)]

        where

        (ui,str')= span(/='@') str1
        i        = drop (length anonymous) ui
        (w,str2) = span (not . isSeparator) $ tail str'
        newVar _= unsafePerformIO  $ newEmptyMVar


     readsPrec _ str= error $ "parse error in Token read from: "++ str

instance Serializable Token  where
  serialize  = B.pack . show
  deserialize= read . B.unpack
  setPersist =   \_ -> Just filePersist

iorefqmap= unsafePerformIO  . newMVar $ M.empty

addTokenToList t@Token{..} =
   modifyMVar_ iorefqmap $ \ map ->
     return $ M.insert  ( tind  ++ twfname  ++ tuser ) t map

deleteTokenInList t@Token{..} =
   modifyMVar_ iorefqmap $ \ map ->
     return $ M.delete  (tind  ++ twfname  ++ tuser) map

getToken msg=  do
      qmap  <- readMVar iorefqmap
      let u= puser msg ; w= pwfname msg ; i=pind msg; ppath=pwfPath msg;penv= getParams msg
      let mqs = M.lookup ( i  ++ w  ++ u) qmap
      case mqs of
              Nothing  -> do
                 q  <- newEmptyMVar  -- `debug` (i++w++u)
                 qr <- newEmptyMVar
                 pblock <- newMVar True
                 let token= Token w u i  ppath penv pblock q qr
                 addTokenToList token
                 return token

              Just token -> return token{tpath= ppath, tenv= penv}


type Flow= (Token -> Workflow IO ())

data HttpData = HttpData [(SB.ByteString,SB.ByteString)]  [Cookie] ByteString | Error  ByteString deriving (Typeable, Show)


instance Monoid HttpData where
 mempty= HttpData [] [] B.empty
 mappend (HttpData h c s) (HttpData h' c' s')= HttpData (h++h') (c++ c') $ mappend s s'

-- | List of (wfname, workflow) pairs, to be scheduled depending on the message's pwfname
type ProcList = WorkflowList IO Token ()


data Req  = forall a.( Processable a, Typeable a)=> Req a   deriving Typeable

type Params =  [(String,String)]

class Processable a where
     pwfname :: a -> String
     pwfname s= Prelude.head $ pwfPath s 
     pwfPath :: a -> [String]
     puser :: a -> String
     pind :: a -> String
     getParams :: a -> Params

instance Processable Token where
     pwfname = twfname
     pwfPath = tpath
     puser = tuser
     pind = tind
     getParams = tenv

instance Processable  Req   where 
    pwfname (Req x)= pwfname x
    pwfPath (Req x)= pwfPath x
    puser (Req x)= puser x
    pind (Req x)= pind x   
    getParams (Req x)= getParams  x
--    getServer (Req x)= getServer  x
--    getPort (Req x)= getPort  x 

data Resp  = Fragm HttpData
           | EndFragm HttpData
           | Resp HttpData




-- | The anonymous user
anonymous= "anon#"

-- | It is the path of the root flow
noScriptRef= unsafePerformIO $ newIORef "noscript"

noScript= unsafePerformIO $ readIORef noScriptRef

-- | set the flow to be executed when the URL has no path. The home page.
--
-- By default it is "noscript".
-- Although it is changed by `runNavigation` to his own flow name.
setNoScript scr= writeIORef noScriptRef scr

{-
instance  (Monad m, Show a) => Traceable (Workflow m a) where
       debugf iox str = do
              x <- iox
              return $ debug x (str++" => Workflow "++ show x)
-}
-- | send a complete response 
--send ::   Token  -> HttpData -> IO()
send  t@(Token _ _ _ _ _ _ _ qresp) msg=   do
      ( putMVar qresp  . Resp $  msg )   -- !> ("<<<<< send "++ show t) 

sendFlush t msg=  flushRec t >>  send t msg      -- !> "sendFlush "

-- | send a response fragment. Useful for streaming. the last packet must be sent trough 'send'
sendFragment ::  Token  -> HttpData -> IO()
sendFragment (Token _ _ _ _ _ _ _ qresp) msg=   putMVar qresp  . Fragm $  msg

{-# DEPRECATED sendEndFragment "use \"send\" to end a fragmented response instead" #-}
sendEndFragment :: Token -> HttpData -> IO()
sendEndFragment (Token _ _ _ _ _ _ _ qresp) msg=  putMVar qresp  $ EndFragm   msg

--emptyReceive (Token  queue _  _)= emptyQueue queue
receive ::  Typeable a => Token -> IO a
receive t= receiveReq t >>= return  . fromReq

flushResponse t@(Token _ _ _ _ _ _ _ qresp)= tryTakeMVar qresp


flushRec t@(Token _ _ _ _ _ _ queue _)= tryTakeMVar  queue   -- !> "flushRec"

receiveReq ::  Token -> IO Req
receiveReq t@(Token _ _ _ _ _ _ queue  _)= do
 r <-   readMVar queue      -- !> (">>>>>> receiveReq ")
 return r                   -- !> "receiveReq >>>>"

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
      delWFHistory1 statKey                               -- `debug` "delWFHistory"
      


-- | executes a simple request-response computation that receive the params and return a response
--
-- It is used with `addMessageFlows`
--
-- There is a higuer level version @wstateless@ in "MFLow.Forms"
stateless ::  (Params -> IO HttpData) -> Flow
stateless f = transient proc
  where
  proc t@(Token _ _ _ _ _ _ queue qresp) = loop t queue qresp
  loop t queue qresp=do
    req <- takeMVar queue                       -- !> (">>>>>> stateless " ++ thread t)
    resp <- f (getParams req)
    (putMVar qresp  $ Resp  resp  ) -- !> ("<<<<<< stateless " ++thread t)
    loop t queue qresp                          -- !>  ("enviado stateless " ++ thread t)



-- | Executes a monadic computation that send and receive messages, but does
-- not store its state in permanent storage. The process once stopped, will restart anew 
--
---- It is used with `addMessageFlows` `hackMessageFlow` or `waiMessageFlow`
transient :: (Token -> IO ()) -> Flow   
transient f=  unsafeIOtoWF . f -- WF(\s -> f t>>= \x-> return (s, x) )


_messageFlows :: MVar (WorkflowList  IO Token ()) --  MVar (M.Map String (Token -> Workflow IO ()))
_messageFlows= unsafePerformIO $ newMVar emptyFList
  where
  emptyFList= M.empty  :: WorkflowList  IO Token ()

-- | add a list of flows to be scheduled. Each entry in the list is a pair @(path, flow)@
addMessageFlows wfs=  modifyMVar_ _messageFlows(\ms ->  return $ M.union (M.fromList $ map flt wfs)ms)
  where flt ("",f)= (noScript,f)
        flt e= e

-- | return the list of the scheduler
getMessageFlows = readMVar _messageFlows

delMessageFlow wfname= modifyMVar_ _messageFlows (\ms -> return $ M.delete wfname ms)


sendToMF Token{..} msg= putMVar tsendq (Req msg)  -- !> "sendToMF"

--recFromMF :: (Typeable a,  Typeable c, Processable a) => Token -> a -> IO c
recFromMF t@Token{..}  = do  
    m <-  takeMVar trecq                          -- !> "recFromMF <<<<<< "
    case m  of
        Resp r  ->  return  r                      -- !> "<<<<<<   recFromMF"
        Fragm r -> do
                   result <- getStream   r
                   return  result

    where
    getStream r =  do
         mr <-  takeMVar trecq 
         case mr of
            Fragm h -> do
                 rest <- unsafeInterleaveIO $  getStream  h
                 let result=  mappend  r   rest
                 return  result 
            EndFragm h -> do
                 let result=  mappend r   h
                 return  result

            Resp h -> do
                 let result=  mappend r   h
                 return  result




-- | The scheduler creates a Token with every `Processable`
-- message that arrives and send the mesage to the appropriate flow, then wait for the response
-- and return it.
--
-- It is the core of the application server. "MFLow.Wai" and "MFlow.Hack" use it
msgScheduler
  :: (Typeable a,Processable a)
  => a  -> IO (HttpData, ThreadId)
msgScheduler x  = do
  token <- getToken x
  th <- myThreadId
  let wfname = takeWhile (/='/') $ pwfname x
  criticalSection (tblock token) $ do
     sendToMF token x                             -- !> show th                             
     th <- startMessageFlow wfname token     
     r  <- recFromMF token                          
     return (r,th)                                --  !> let HttpData _ _ r1=r in B.unpack r1 
  where
  criticalSection mv f= bracket
      (takeMVar mv)
      (putMVar mv)
      $ const $ f
      
  --start the flow if not started yet
  startMessageFlow wfname token = 
   forkIO $ do
        wfs <- getMessageFlows
        r   <- startWF wfname  token   wfs          -- !>( "init wf " ++ wfname)
        case r of
          Left NotFound -> do
                 (sendFlush token =<<  serveFile  (pwfname x))
                    `CE.catch` \(e:: CE.SomeException) -> do
                       showError wfname token (show e)
--                     sendFlush token (Error NotFound $ "Not found: " <> pack wfname)
                       deleteTokenInList token

          Left AlreadyRunning -> return ()                    -- !> ("already Running " ++ wfname)

          Left Timeout -> do
              hFlush stdout                                       -- !>  ("TIMEOUT in msgScheduler" ++ (show $ unsafePerformIO myThreadId))
              deleteTokenInList token
             
          Left (WFException e)-> do
              showError wfname token e
              moveState wfname token token{tind= "error/"++tuser token}
              deleteTokenInList token                       -- !> "DELETETOKEN"
              
              
          Right _ ->  delMsgHistory token >> return ()      -- !> ("finished " ++ wfname)



showError wfname token@Token{..} e= do
   t <- return . calendarTimeToString =<< toCalendarTime =<< getClockTime
   let msg= errorMessage t e tuser (Prelude.concat $ intersperse "/" tpath) tenv
   logError  msg
   fresp <- getNotFoundResponse
   let admin=  getAdminName
   sendFlush token . Error $ fresp (tuser== admin)  $  Prelude.concat[ "<br/>"++ s | s <- lines msg]


errorMessage t e u path env=
     "\n---------------------ERROR-------------------------\
     \\nTIME=" ++ t ++"\n\n" ++
     e++
     "\n\nUSER= " ++ u ++
     "\n\nPATH= " ++ path ++
     "\n\nREQUEST:\n\n" ++
     show env

line= unsafePerformIO $ newMVar ()

logError err= do
     takeMVar line
     putStrLn err
     hSeek hlog SeekFromEnd 0
     hPutStrLn hlog err
     hFlush hlog
     putMVar line ()

logFileName= "errlog"



-- | The handler of the error log
hlog= unsafePerformIO $ openFile logFileName ReadWriteMode

------ USER MANAGEMENT -------

data Auth = Auth{
   uregister ::  UserStr -> PasswdStr -> (IO (Maybe String)),
   uvalidate ::  UserStr -> PasswdStr -> (IO (Maybe String))}

_authMethod= unsafePerformIO $ newIORef $ Auth tCacheRegister tCacheValidate

-- | set an authentication method. That includes the registration and validation calls.
-- both return Nothing if sucessful. Otherwise they return a text mesage explaining the failure
setAuthMethod auth= writeIORef _authMethod auth

getAuthMethod = readIORef _authMethod


data User= User
            { userName :: String
            , upassword :: String
            } deriving (Read, Show, Typeable)


eUser= User (error1 "username") (error1 "password")

error1 s= error $ s ++ " undefined"

userPrefix= "user/"
instance Indexable User where
   key User{userName= user}= keyUserName user

-- | Return  the key name of an user
keyUserName n= userPrefix++n

instance  Serializable User where
  serialize=  B.pack . show
  deserialize=   read . B.unpack
  setPersist =   \_ -> Just filePersist
  
-- | Register an user/password 
tCacheRegister ::  String -> String  -> IO (Maybe String)
tCacheRegister user password= tCacheRegister' 14 user password

tCacheRegister' strength user password= do
     salted_password <- makePassword (SB.pack password) strength
     atomically $ do
         let newuser = User user (SB.unpack salted_password)
         withSTMResources [newuser] $ doit newuser
     where
         doit newuser [Just (User _ _)] = resources{toReturn= Just "user already exist"}
         doit newuser [Nothing] = resources{toAdd= [newuser],toReturn= Nothing}


--     withSTMResources [newuser]  doit
--     where
--     newuser= User user password
--     doit [Just (User _ _)] = resources{toReturn= Just "user already exist"}
--     doit [Nothing] = resources{toAdd= [newuser],toReturn= Nothing}

tCacheValidate ::  UserStr -> PasswdStr -> IO (Maybe String)
tCacheValidate  u p =
    let user= eUser{userName=u}
    in atomically
     $ withSTMResources [user]
     $ \ mu -> case mu of
         [Nothing] -> resources{toReturn= err }
         [Just u@(User _ pass )] -> resources{toReturn =
               case verifyPassword (SB.pack p) (SB.pack pass)
                    || pass== p of   -- for backward compatibility for unhashed passwords
                  True -> Nothing
                  False -> err
               }
     where
     err= Just  "Username or password invalid"

-- | register an user with the auth Method
userRegister :: MonadIO m => UserStr -> PasswdStr -> m (Maybe String)
userRegister !u !p= liftIO $ do
   Auth reg _ <- getAuthMethod :: IO Auth
   reg u p


newtype Config= Config1 (M.Map String String) deriving (Read,Show,Typeable)


data Config0 = Config{cadmin :: UserStr           -- ^ Administrator name
                     ,cjqueryScript :: String     -- ^ URL of jquery
                     ,cjqueryCSS    :: String     -- ^ URL of jqueryCSS
                     ,cjqueryUI     :: String     -- ^ URL of jqueryUI
                     ,cnicEditUrl    :: String    -- ^ URL of the nicEdit  editor
                     }
                    deriving (Read, Show, Typeable)


change :: Config0 -> Config
change Config{..} = Config1 $ M.fromList
            [("cadmin",cadmin )
            ,("cjqueryScript", cjqueryScript)
            ,("cjqueryCSS",cjqueryCSS)
            ,("cjqueryUI",cjqueryUI)
            ,("cnicEditUrl",cnicEditUrl)]

config :: M.Map String String
config= unsafePerformIO $! do
  Config1 c <- atomically $! readConfig
  return c

readConfig=  readDBRef rconf `onNothing`  return (Config1 $ M.fromList []) -- defConfig

readOld :: ByteString -> Config
readOld s= (change . read . B.unpack $ s)

keyConfig= "mflow.config"
instance Indexable Config where key _= keyConfig

rconf :: DBRef Config
rconf= getDBRef keyConfig

instance  Serializable Config where
  serialize (Config1 c)= B.pack  $ "Config1 (fromList[\n" <> (concat . intersperse ",\n" $ map show (M.toList c)) <> "])"
  deserialize s = unsafePerformIO $  (return $! read $! B.unpack s)
                   `CE.catch` \(e :: SomeException) ->  return (readOld s)
  setPersist = \_ -> Just filePersist

-- | read a config variable from the config file \"mflow.config\". if it is not set, uses the second parameter and
-- add it to the configuration list, so next time the administrator can change it in the configuration file
getConfig k v=  case M.lookup k config of
     Nothing -> unsafePerformIO $ setConfig k v >> return v
     Just s  -> s

-- | set an user-defined config variable
setConfig k v= atomically $ do
     Config1 conf <-  readConfig
     writeDBRef rconf $ Config1 $ M.insert k v conf


-- user ---

type UserStr= String
type PasswdStr= String


-- | set the Administrator user and password.
-- It must be defined in Main , before any configuration parameter is read, before the execution
-- of any flow
setAdminUser :: MonadIO m => UserStr -> PasswdStr -> m ()
setAdminUser user password= liftIO $  do
  userRegister user password
  setConfig "cadmin" user


getAdminName= getConfig "cadmin"  "admin"


--------------- ERROR RESPONSES --------

defNotFoundResponse isAdmin msg= fresp $
     case isAdmin of
           True -> B.pack msg
           _    -> "The administrator has been notified"
  where
  fresp msg=
   "<html><h4>Error 404: Page not found or error ocurred</h4> <p style=\"font-family:courier\">" <> msg <>"</p>" <>
   "<br/>" <> opts <> "<br/><a href=\"/\" >press here to go home</a></html>"

   
  paths= Prelude.map B.pack . M.keys $ unsafePerformIO getMessageFlows
  opts=  "options: " <> B.concat (Prelude.map  (\s ->
                          "<a href=\"/"<>  s <>"\">"<> s <>"</a>, ") $ filter (\s -> B.head s /= '_') paths)

notFoundResponse=  unsafePerformIO $ newIORef defNotFoundResponse

-- | set the  404 "not found" response.
--
-- The parameter is as follows:
--    (Bool        Either if the user is Administrator or not
--  -> String      The error string
--  -> HttpData)   The response. See `defNotFoundResponse` code for an example

setNotFoundResponse :: 
    (Bool    
  -> String     
  -> ByteString)  
  -> IO ()

setNotFoundResponse f= liftIO $ writeIORef notFoundResponse  f
getNotFoundResponse= liftIO $ readIORef notFoundResponse

--------------- BASIC BYTESTRING TAGS -------------------


type Attribs= [(String,String)]
-- | Writes a XML tag in a ByteString. It is the most basic form of formatting. For
-- more sophisticated formatting , use "MFlow.Forms.XHtml" or "MFlow.Forms.HSP".
btag :: String -> Attribs  -> ByteString -> ByteString
btag t rs v= "<" <> pt <> attrs rs <> ">" <> v <> "</" <> pt <> ">"
 where
 pt= B.pack t
 attrs []= B.empty
 attrs rs=  B.pack $ concatMap(\(n,v) -> (' ' :   n) ++ "=\"" ++ v++ "\"" ) rs

-- |
-- > bhtml ats v= btag "html" ats v
bhtml :: Attribs -> ByteString -> ByteString
bhtml ats v= btag "html" ats v


-- |
-- > bbody ats v= btag "body" ats v
bbody :: Attribs -> ByteString -> ByteString
bbody ats v= btag "body" ats v

addAttrs :: ByteString -> Attribs -> ByteString
addAttrs (Chunk "<" (Chunk tag rest)) rs=
   Chunk "<"(Chunk tag  (B.pack $ concatMap(\(n,v) -> (' ' :   n) ++ "=" ++  v ) rs))  <> rest

addAttrs other _ = error  $ "addAttrs: byteString is not a tag: " ++ show other


------------------- FILE SERVER -----------

-- | Set the path of the files in the web server. The links to the files are relative to it.
-- The files are cached (memoized) according with the "Data.TCache" policies in the program space. This avoid the blocking of
-- the efficient GHC threads by frequent IO calls.So it enhances the performance
-- in the context of heavy concurrence.
-- It uses 'Data.TCache.Memoization'. 
-- The caching-uncaching follows the `setPersist` criteria
setFilesPath :: MonadIO m => String -> m ()
setFilesPath !path= liftIO $ writeIORef rfilesPath path

rfilesPath= unsafePerformIO $ newIORef "files/"

serveFile path'= do
     when(let hpath= Prelude.head path' in hpath == '/' || hpath =='\\') $ error noperm
     when(not(".." `isSuffixOf` path') && ".." `isInfixOf` path') $ error noperm
     filesPath <- readIORef rfilesPath
     let path= filesPath ++ path'
     mr <-  cachedByKey path 0  $   (B.readFile  path >>=  return . Just) `CE.catch` ioerr (return Nothing)
     case mr of
      Nothing -> error "not found" -- return $ HttpData  [setMime "text/plain"] [] $ pack $  "not accessible"
      Just r ->
         let ext  = reverse . takeWhile (/='.') $ reverse path
             mmime= lookup (map toLower ext) mimeTable
             mime = case mmime of Just m -> m; Nothing -> "application/octet-stream"

         in return $ HttpData  [setMime mime, ("Cache-Control", "max-age=360000")] [] r
   where
   noperm= "no permissions"
   ioerr x= \(e :: CE.IOException) ->  x
   setMime x= ("Content-Type",x)


--------------------- FLOW ID GENERATOR ------------

data NFlow= NFlow !Integer deriving (Read, Show, Typeable)



instance Indexable NFlow where
  key _= "Flow"

instance  Serializable NFlow where
  serialize=  B.pack . show
  deserialize=   read . B.unpack
  setPersist =   \_ -> Just filePersist

rflow= getDBRef . key $ NFlow undefined

newFlow=  do
        TOD t _ <- getClockTime
        atomically $ do 
                    NFlow n <- readDBRef rflow `onNothing` return (NFlow 0)
                    writeDBRef rflow . NFlow $ n+1
                    return . SB.pack . show $ t + n


mimeTable=[
    ("html",	"text/html"),
    ("htm",	"text/html"),
    ("txt",	"text/plain"),
    ("hs",      "text/plain"),
    ("lhs",      "text/plain"), 
    ("jpeg",	"image/jpeg"),
    ("pdf",	"application/pdf"),
    ("js",	"application/x-javascript"),
    ("gif",	"image/gif"),
    ("bmp",	"image/bmp"),
    ("ico",	"image/x-icon"),
    ("doc",	"application/msword"),
    ("jpg",	"image/jpeg"),
    ("eps",	"application/postscript"),
    ("zip",	"application/zip"),
    ("exe",	"application/octet-stream"),
    ("tif",	"image/tiff"),
    ("tiff",	"image/tiff"),
    ("mov",	"video/quicktime"),
    ("movie",	"video/x-sgi-movie"),
    ("mp2",	"video/mpeg"),
    ("mp3",	"audio/mpeg"),
    ("mpa",	"video/mpeg"),
    ("mpe",	"video/mpeg"),
    ("mpeg",	"video/mpeg"),
    ("mpg",	"video/mpeg"),
    ("mpp",	"application/vnd.ms-project"),
    ("323",	"text/h323"),
    ("*",	"application/octet-stream"),
    ("acx",	"application/internet-property-stream"),
    ("ai",	"application/postscript"),
    ("aif",	"audio/x-aiff"),
    ("aifc",	"audio/x-aiff"),
    ("aiff",	"audio/x-aiff"),
    ("asf",	"video/x-ms-asf"),
    ("asr",	"video/x-ms-asf"),
    ("asx",	"video/x-ms-asf"),
    ("au",	"audio/basic"),
    ("avi",	"video/x-msvideo"),
    ("axs",	"application/olescript"),
    ("bas",	"text/plain"),
    ("bcpio",	"application/x-bcpio"),
    ("bin",	"application/octet-stream"),
    ("c",	"text/plain"),
    ("cat",	"application/vnd.ms-pkiseccat"),
    ("cdf",	"application/x-cdf"),
    ("cdf",	"application/x-netcdf"),
    ("cer",	"application/x-x509-ca-cert"),
    ("class",	"application/octet-stream"),
    ("clp",	"application/x-msclip"),
    ("cmx",	"image/x-cmx"),
    ("cod",	"image/cis-cod"),
    ("cpio",	"application/x-cpio"),
    ("crd",	"application/x-mscardfile"),
    ("crl",	"application/pkix-crl"),
    ("crt",	"application/x-x509-ca-cert"),
    ("csh",	"application/x-csh"),
    ("css",	"text/css"),
    ("dcr",	"application/x-director"),
    ("der",	"application/x-x509-ca-cert"),
    ("dir",	"application/x-director"),
    ("dll",	"application/x-msdownload"),
    ("dms",	"application/octet-stream"),
    ("dot",	"application/msword"),
    ("dvi",	"application/x-dvi"),
    ("dxr",	"application/x-director"),
    ("eps",	"application/postscript"),
    ("etx",	"text/x-setext"),
    ("evy",	"application/envoy"),
    ("fif",	"application/fractals"),
    ("flr",	"x-world/x-vrml"),
    ("gtar",	"application/x-gtar"),
    ("gz",	"application/x-gzip"),
    ("h",	"text/plain"),
    ("hdf",	"application/x-hdf"),
    ("hlp",	"application/winhlp"),
    ("hqx",	"application/mac-binhex40"),
    ("hta",	"application/hta"),
    ("htc",	"text/x-component"),
    ("htt",	"text/webviewhtml"),
    ("ief",	"image/ief"),
    ("iii",	"application/x-iphone"),
    ("ins",	"application/x-internet-signup"),
    ("isp",	"application/x-internet-signup"),
    ("jfif",	"image/pipeg"),
    ("jpe",	"image/jpeg"),
    ("latex",	"application/x-latex"),
    ("lha",	"application/octet-stream"),
    ("lsf",	"video/x-la-asf"),
    ("lsx",	"video/x-la-asf"),
    ("lzh",	"application/octet-stream"),
    ("m13",	"application/x-msmediaview"),
    ("m14",	"application/x-msmediaview"),
    ("m3u",	"audio/x-mpegurl"),
    ("man",	"application/x-troff-man"),
    ("mdb",	"application/x-msaccess"),
    ("me",	"application/x-troff-me"),
    ("mht",	"message/rfc822"),
    ("mhtml",	"message/rfc822"),
    ("mid",	"audio/mid"),
    ("mny",	"application/x-msmoney"),
    ("mpv2",	"video/mpeg"),
    ("ms",	"application/x-troff-ms"),
    ("msg",	"application/vnd.ms-outlook"),
    ("mvb",	"application/x-msmediaview"),
    ("nc",	"application/x-netcdf"),
    ("nws",	"message/rfc822"),
    ("oda",	"application/oda"),
    ("p10",	"application/pkcs10"),
    ("p12",	"application/x-pkcs12"),
    ("p7b",	"application/x-pkcs7-certificates"),
    ("p7c",	"application/x-pkcs7-mime"),
    ("p7m",	"application/x-pkcs7-mime"),
    ("p7r",	"application/x-pkcs7-certreqresp"),
    ("p7s",	"application/x-pkcs7-signature"),
    ("png",     "image/png"),
    ("pbm",	"image/x-portable-bitmap"),
    ("pfx",	"application/x-pkcs12"),
    ("pgm",	"image/x-portable-graymap"),
    ("pko",	"application/ynd.ms-pkipko"),
    ("pma",	"application/x-perfmon"),
    ("pmc",	"application/x-perfmon"),
    ("pml",	"application/x-perfmon"),
    ("pmr",	"application/x-perfmon"),
    ("pmw",	"application/x-perfmon"),
    ("pnm",	"image/x-portable-anymap"),
    ("pot",	"application/vnd.ms-powerpoint"),
    ("ppm",	"image/x-portable-pixmap"),
    ("pps",	"application/vnd.ms-powerpoint"),
    ("ppt",	"application/vnd.ms-powerpoint"),
    ("prf",	"application/pics-rules"),
    ("ps",	"application/postscript"),
    ("pub",	"application/x-mspublisher"),
    ("qt",	"video/quicktime"),
    ("ra",	"audio/x-pn-realaudio"),
    ("ram",	"audio/x-pn-realaudio"),
    ("ras",	"image/x-cmu-raster"),
    ("rgb",	"image/x-rgb"),
    ("rmi",	"audio/mid"),
    ("roff",	"application/x-troff"),
    ("rtf",	"application/rtf"),
    ("rtx",	"text/richtext"),
    ("scd",	"application/x-msschedule"),
    ("sct",	"text/scriptlet"),
    ("setpay",	"application/set-payment-initiation"),
    ("setreg",	"application/set-registration-initiation"),
    ("sh",	"application/x-sh"),
    ("shar",	"application/x-shar"),
    ("sit",	"application/x-stuffit"),
    ("snd",	"audio/basic"),
    ("spc",	"application/x-pkcs7-certificates"),
    ("spl",	"application/futuresplash"),
    ("src",	"application/x-wais-source"),
    ("sst",	"application/vnd.ms-pkicertstore"),
    ("stl",	"application/vnd.ms-pkistl"),
    ("stm",	"text/html"),
    ("sv4cpio",	"application/x-sv4cpio"),
    ("sv4crc",	"application/x-sv4crc"),
    ("svg",	"image/svg+xml"),
    ("swf",	"application/x-shockwave-flash"),
    ("t",	"application/x-troff"),
    ("tar",	"application/x-tar"),
    ("tcl",	"application/x-tcl"),
    ("tex",	"application/x-tex"),
    ("texi",	"application/x-texinfo"),
    ("texinfo",	"application/x-texinfo"),
    ("tgz",	"application/x-compressed"),
    ("tr",	"application/x-troff"),
    ("trm",	"application/x-msterminal"),
    ("tsv",	"text/tab-separated-values"),
    ("uls",	"text/iuls"),
    ("ustar",	"application/x-ustar"),
    ("vcf",	"text/x-vcard"),
    ("vrml",	"x-world/x-vrml"),
    ("wav",	"audio/x-wav"),
    ("wcm",	"application/vnd.ms-works"),
    ("wdb",	"application/vnd.ms-works"),
    ("wks",	"application/vnd.ms-works"),
    ("wmf",	"application/x-msmetafile"),
    ("wps",	"application/vnd.ms-works"),
    ("wri",	"application/x-mswrite"),
    ("wrl",	"x-world/x-vrml"),
    ("wrz",	"x-world/x-vrml"),
    ("xaf",	"x-world/x-vrml"),
    ("xbm",	"image/x-xbitmap"),
    ("xla",	"application/vnd.ms-excel"),
    ("xlc",	"application/vnd.ms-excel"),
    ("xlm",	"application/vnd.ms-excel"),
    ("xls",	"application/vnd.ms-excel"),
    ("xlt",	"application/vnd.ms-excel"),
    ("xlw",	"application/vnd.ms-excel"),
    ("xof",	"x-world/x-vrml"),
    ("xpm",	"image/x-xpixmap"),
    ("xwd",	"image/x-xwindowdump"),
    ("z",	"application/x-compress")

 ]

