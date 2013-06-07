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
             -XExistentialQuantification
             -XScopedTypeVariables
             -XFlexibleInstances
             -XUndecidableInstances
             -XMultiParamTypeClasses
             -XGeneralizedNewtypeDeriving
             -XFlexibleContexts
             -XOverlappingInstances
             -XRecordWildCards
#-}

module MFlow.Forms.Internals where
import MFlow
import MFlow.Cookies
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Data.ByteString.Lazy.Char8 as B(ByteString,cons,pack,unpack,append,empty,fromChunks)
import Data.Typeable
import Data.RefSerialize hiding((<|>))
import Data.TCache
import Data.TCache.Memoization
import Data.TCache.DefaultPersistence
import Data.Dynamic
import qualified Data.Map as M
import Unsafe.Coerce
import Control.Workflow as WF
import Control.Monad.Identity
import Data.List
import System.IO.Unsafe
import Control.Concurrent.MVar

--import Debug.Trace
--(!>)= flip trace

instance Serialize a => Serializable a where
  serialize=  runW . showp
  deserialize=   runR readp

type UserStr= String
type PasswdStr= String

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

-- | Register an user/password 
userRegister :: MonadIO m => String -> String  -> m (DBRef User)
userRegister user password  = liftIO . atomically $ newDBRef $ User user password

-- | Authentication against `userRegister`ed users.
-- to be used with `validate`
userValidate :: (FormInput view,MonadIO m) => (UserStr,PasswdStr) -> m (Maybe view)
userValidate (u,p) =
    let user= eUser{userName=u}
    in liftIO $ atomically
     $ withSTMResources [user]
     $ \ mu -> case mu of
         [Nothing] -> resources{toReturn= err }
         [Just (User _ pass )] -> resources{toReturn= 
               case pass==p  of
                 True -> Nothing
                 False -> err
               }

     where
     err= Just . fromStr $ "Username or password invalid"

data Config = Config UserStr deriving (Read, Show, Typeable)

keyConfig= "mflow.config"
instance Indexable Config where key _= keyConfig
rconf= getDBRef keyConfig

setAdminUser :: MonadIO m => UserStr -> PasswdStr -> m ()
setAdminUser user password= liftIO $  atomically $ do
  newDBRef $ User user password
  writeDBRef rconf $ Config user

getAdminName :: MonadIO m => m UserStr
getAdminName= liftIO $ atomically ( readDBRef rconf `onNothing` error "admin user not set" ) >>= \(Config u) -> return u


data FailBack a = BackPoint a | NoBack a | GoBack   deriving (Show,Typeable)


instance (Serialize a) => Serialize (FailBack a ) where
   showp (BackPoint x)= insertString (pack iCanFailBack) >> showp x
   showp (NoBack x)= insertString (pack noFailBack) >> showp x
   showp GoBack = insertString (pack repeatPlease)

   readp = choice [icanFailBackp,repeatPleasep,noFailBackp]
    where
    noFailBackp   = {-# SCC "deserialNoBack" #-} symbol noFailBack >> readp >>= return . NoBack
    icanFailBackp = {-# SCC "deserialBackPoint" #-} symbol iCanFailBack >> readp >>= return . BackPoint
    repeatPleasep = {-# SCC "deserialbackPlease" #-}  symbol repeatPlease  >> return  GoBack

iCanFailBack= "B"
repeatPlease= "G"
noFailBack= "N"

newtype BackT m a = BackT { runBackT :: m (FailBack a ) }

instance (Monad m, HandleBacktracking s m)=> Monad (BackT  m) where
    fail   _ = BackT . return $ GoBack
    return x = BackT . return $ NoBack x
    x >>= f  = BackT $ loop
     where
     loop = do
        s <- get
        v <- runBackT x                          -- !> "loop"
        case v of
            NoBack y  -> runBackT (f y)         -- !> "runback"
            BackPoint y  -> do
                 z <- runBackT (f y)           -- !> "BACK"
                 case z of
                  GoBack   -> handle s >> loop              -- !> "GoBack"
                  other -> return other
            GoBack  ->  return  $ GoBack

class MonadState s m => HandleBacktracking s m where
   handle :: s -> m ()

--instance Monad m => HandleState m where
--   handle= return ()

fromFailBack (NoBack  x)   = x
fromFailBack (BackPoint  x)= x

toFailBack x= NoBack x

{-# NOINLINE breturn  #-}

-- | Use this instead of return to return from a computation with ask statements
--
-- This way when the user press the back button, the computation will execute back, to
-- the returned code, according with the user navigation.
breturn :: (Monad m) => a -> FlowM v m a
breturn = flowM . BackT . return . BackPoint           -- !> "breturn"


instance (HandleBacktracking s m,MonadIO m) => MonadIO (BackT  m) where
  liftIO f= BackT $ liftIO  f >>= \ x -> return $ NoBack x

instance (Monad m,Functor m) => Functor (BackT m) where
  fmap f g= BackT $ do
     mr <- runBackT g
     case mr of
      BackPoint x  -> return . BackPoint $ f x
      NoBack x     -> return . NoBack $ f x
      GoBack       -> return $ GoBack


liftBackT f = BackT $ f  >>= \x ->  return $ NoBack x
instance MonadTrans BackT where
  lift f = BackT $ f  >>= \x ->  return $ NoBack x


instance (HandleBacktracking s m,MonadState s m) => MonadState s (BackT m) where
   get= lift get                                -- !> "get"
   put= lift . put

type WState view m = StateT (MFlowState view) m
type FlowMM view m=  BackT (WState view m)

data FormElm view a = FormElm [view] (Maybe a) deriving Typeable

instance Serialize a => Serialize (FormElm view a) where
   showp (FormElm _ x)= showp x
   readp= readp >>= \x -> return $ FormElm  [] x


newtype View v m a = View { runView :: WState v m (FormElm v a)}

instance Monad m => HandleBacktracking (MFlowState v) (WState v m) where
   handle st= do
        MFlowState{..} <- get
        put  st{mfEnv= mfEnv,linkDepth= linkDepth,mfSequence= mfSequence}

newtype FlowM v m a= FlowM {runFlowM :: FlowMM v m a} deriving (Monad,MonadIO,MonadState(MFlowState v))
flowM= FlowM
--runFlowM= runView


instance (FormInput v,Serialize a)
   => Serialize (a,MFlowState v) where
   showp (x,s)= case mfDebug s of
      False -> showp x
      True  -> showp(x, mfEnv s)
   readp= choice[nodebug, debug]
    where
    nodebug= readp  >>= \x -> return  (x, mFlowState0)
    debug=  do
     (x,env) <- readp
     return  (x,mFlowState0{mfEnv= env})
    

instance Functor (FormElm view ) where
  fmap f (FormElm form x)= FormElm form (fmap f x)

instance  (Monad m,Functor m) => Functor (View view m) where
  fmap f x= View $   fmap (fmap f) $ runView x

  
instance (Functor m, Monad m) => Applicative (View view m) where
  pure a  = View $  return (FormElm  [] $ Just a)
  View f <*> View g= View $
                   f >>= \(FormElm form1 k) ->
                   g >>= \(FormElm form2 x) ->
                   return $ FormElm (form1 ++ form2) (k <*> x) 

instance (Functor m, Monad m) => Alternative (View view m) where
  empty= View $ return $ FormElm [] Nothing
  View f <|> View g= View $ do
                   FormElm form1 k <- f
                   FormElm form2 x <- g
                   return $ FormElm (form1 ++ form2) (k <|> x)


instance  (Monad m) => Monad (View view m) where
    View x >>= f = View $ do
                   FormElm form1 mk <- x
                   case mk of
                     Just k  -> do
                       FormElm form2 mk <- runView $ f k
                       return $ FormElm (form1 ++ form2) mk
                     Nothing -> return $ FormElm form1 Nothing

    return= View .  return . FormElm  [] . Just 

--instance  (Monad m) => Monad (FlowM view m) where
--  --View view m a-> (a -> View view m b) -> View view m b
--    FlowM x >>= f = FlowM $ do
--                   FormElm _ mk <- x
--                   case mk of
--                     Just k  -> do
--                       FormElm _ mk <- runFlowM $ f k
--                       return $ FormElm [] mk
--                     Nothing -> return $ FormElm [] Nothing
--
--    return= FlowM .  return . FormElm  [] . Just

instance MonadTrans (View view) where
  lift f = View $  (lift  f) >>= \x ->  return $ FormElm [] $ Just x

instance MonadTrans (FlowM view) where
  lift f = FlowM $ lift (lift  f) >>= \x ->  return x



instance  (Monad m)=> MonadState (MFlowState view) (View view m) where
  get = View $  get >>= \x ->  return $ FormElm [] $ Just x
  put st = View $  put st >>= \x ->  return $ FormElm [] $ Just x

--instance  (Monad m)=> MonadState (MFlowState view) (FlowM view m) where
--  get = FlowM $  get >>= \x ->  return $ FormElm [] $ Just x
--  put st = FlowM $  put st >>= \x ->  return $ FormElm [] $ Just x


instance (MonadIO m) => MonadIO (View view m) where
    liftIO io= let x= liftIO io in x `seq` lift x -- to force liftIO==unsafePerformIO onf the Identity monad

-- | Execute the widget in a monad and return the result in another.
changeMonad :: (Monad m, Executable m1)
             => View v m1 a -> View v m a
changeMonad w= View . StateT $ \s ->
    let (r,s')= execute $ runStateT  ( runView w)    s
    in mfSequence s' `seq` return (r,s')


type Lang=  String

data MFlowState view= MFlowState{   
   mfSequence       :: Int,
   mfCached         :: Bool,
   prevSeq          :: [Int],
   onInit           :: Bool,
   inSync           :: Bool,
   mfLang           :: Lang,
   mfEnv            :: Params,
   needForm         :: Bool,
   mfToken          :: Token,
   mfkillTime       :: Int,
   mfSessionTime    :: Integer,
   mfCookies        :: [Cookie],
   mfHttpHeaders    :: Params,
   mfHeader         :: view -> view,
   mfDebug          :: Bool,
   mfRequirements   :: [Requirement],
   mfData           :: M.Map TypeRep Void,
   mfAjax           :: Maybe (M.Map String Void),
   mfSeqCache       :: Int,
   notSyncInAction  :: Bool,
   hasParams        :: Bool,
   linkDepth        :: Int,
   mfPath           :: [String]
   }
   deriving Typeable

type Void = Char

mFlowState0 :: (FormInput view) => MFlowState view
mFlowState0 = MFlowState 0 False [] True  True  "en"
                [] False  (error "token of mFlowState0 used")
                0 0 [] [] stdHeader False [] M.empty  Nothing 0 False  False (-1) []


-- | Set user-defined data in the context of the session.
--
-- The data is indexed by  type in a map. So the user can insert-retrieve different kinds of data
-- in the session context.
--
-- This example define @addHistory@ and @getHistory@ to maintain a Html log in the session of a Flow:
--
-- > newtype History = History ( Html) deriving Typeable
-- > setHistory html= setSessionData $ History html
-- > getHistory= getSessionData `onNothing` return (History mempty) >>= \(History h) -> return h
-- > addHistory html= do
-- >      html' <- getHistory
-- >      setHistory $ html' `mappend` html

setSessionData ::  (Typeable a,MonadState (MFlowState view) m) => a ->m ()  
setSessionData  x=
  modify $ \st -> st{mfData= M.insert  (typeOf x ) (unsafeCoerce x) (mfData st)}

-- | Get the session data of the desired type if there is any.
getSessionData ::  (Typeable a, MonadState (MFlowState view) m) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | Return the user language. Now it is fixed to "en"
getLang ::  MonadState (MFlowState view) m => m String
getLang= gets mfLang

getToken :: MonadState (MFlowState view) m => m Token
getToken= gets mfToken


-- get a parameter form the las received response
getEnv ::  MonadState (MFlowState view) m =>  m Params
getEnv = gets mfEnv

stdHeader v = v


-- | Set the header-footer that will enclose the widgets. It must be provided in the
-- same formatting than them, altrough with normalization to byteStrings any formatting can be used
--
-- This header uses XML trough Haskell Server Pages (<http://hackage.haskell.org/package/hsp>)
--
-- @
-- setHeader $ \c ->
--            \<html\>
--                 \<head\>
--                      \<title\>  my title \</title\>
--                      \<meta name= \"Keywords\" content= \"sci-fi\" /\>)
--                 \</head\>
--                  \<body style= \"margin-left:5%;margin-right:5%\"\>
--                       \<% c %\>
--                  \</body\>
--            \</html\>
-- @
--
-- This header uses "Text.XHtml"
--
-- @
-- setHeader $ \c ->
--           `thehtml`
--               << (`header`
--                   << (`thetitle` << title +++
--                       `meta` ! [`name` \"Keywords\",content \"sci-fi\"])) +++
--                  `body` ! [`style` \"margin-left:5%;margin-right:5%\"] c
-- @
--
-- This header uses both. It uses byteString tags
--
-- @
-- setHeader $ \c ->
--          `bhtml` [] $
--               `btag` "head" [] $
--                     (`toByteString` (thetitle << title) `append`
--                     `toByteString` <meta name= \"Keywords\" content= \"sci-fi\" />) `append`
--                  `bbody` [(\"style\", \"margin-left:5%;margin-right:5%\")] c
-- @

setHeader :: MonadState (MFlowState view) m => (view -> view) ->  m ()
setHeader header= do
  fs <- get
  put fs{mfHeader= header}



-- | Return the current header
getHeader :: (Monad m) => FlowM view m (view -> view)
getHeader= gets mfHeader

-- | Set an HTTP cookie
setCookie :: MonadState (MFlowState view) m
          => String  -- ^ name
          -> String  -- ^ value
          -> String  -- ^ path
          -> Maybe Integer  -- ^ Max-Age in seconds. Nothing for a session cookie
          -> m ()
setCookie n v p me= do
    modify $ \st -> st{mfCookies= (n,v,p,fmap  show me):mfCookies st }

-- | Set an HTTP Response header
setHttpHeader :: MonadState (MFlowState view) m
          => String  -- ^ name
          -> String  -- ^ value
          -> m ()
setHttpHeader n v = do
    modify $ \st -> st{mfHttpHeaders=  (n,v):mfHttpHeaders st }


-- | Set 1) the timeout of the flow execution since the last user interaction.
-- Once passed, the flow executes from the begining. 2). In persistent flows
-- it set the session state timeout for the flow, that is persistent. If the
-- flow is not persistent, it has no effect.
--
-- `transient` flows restart anew.
-- persistent flows (that use `step`) restart at the las saved execution point, unless
-- the session time has expired for the user.
setTimeouts :: Monad m => Int -> Integer -> FlowM view m ()
setTimeouts kt st= do
 fs <- get
 put fs{ mfkillTime= kt, mfSessionTime= st}


getWFName ::   MonadState (MFlowState view) m =>   m String
getWFName = do
 fs <- get
 return . twfname $ mfToken fs

getCurrentUser ::  MonadState (MFlowState view) m=>  m String
getCurrentUser = return . tuser  =<< gets mfToken

type Name= String
type Type= String
type Value= String
type Checked= Bool
type OnClick= Maybe String

normalize ::(Monad m, FormInput v) => View v m a -> View ByteString m a
normalize f=  View .  StateT $ \s ->do
       (FormElm fs mx, s') <-  runStateT  ( runView f) $ unsafeCoerce s
       return  (FormElm (map toByteString fs ) mx,unsafeCoerce s')

--class ToByteString a where
--  toByteString :: a -> ByteString
--
--instance ToByteString a => ToHttpData a where
--  toHttpData = toHttpData . toByteString
--
--instance ToByteString ByteString where
--  toByteString= id
--
--instance ToByteString String where
--  toByteString  =  pack

-- | Minimal interface for defining the basic form combinators in a concrete rendering.
-- defined in this module. see "MFlow.Forms.XHtml" for the instance for @Text.XHtml@ and MFlow.Forms.HSP for an instance
-- form Haskell Server Pages.
class (Monoid view,Typeable view)   => FormInput view where
    toByteString :: view -> ByteString
    toHttpData :: view -> HttpData
    fromStr :: String -> view
    fromStrNoEncode :: String -> view
    ftag :: String -> view  -> view
    inred   :: view -> view
    flink ::  String -> view -> view 
    flink1:: String -> view
    flink1 verb = flink verb (fromStr  verb) 
    finput :: Name -> Type -> Value -> Checked -> OnClick -> view 
    ftextarea :: String -> String -> view
    fselect :: String -> view -> view
    foption :: String -> view -> Bool -> view
    foption1 :: String -> Bool -> view
    foption1   val msel= foption val (fromStr val) msel
    formAction  :: String -> view -> view
    attrs :: view -> Attribs -> view



--instance (MonadIO m) => MonadIO (FlowM view m) where
--    liftIO io= let x= liftIO io in x `seq` lift x -- to force liftIO==unsafePerformIO onf the Identity monad

--instance Executable (View v m) where
--  execute f =  execute $  evalStateT  f mFlowState0


--instance (Monad m, Executable m, Monoid view, FormInput view)
--          => Executable (StateT (MFlowState view) m) where
--   execute f= execute $  evalStateT  f mFlowState0

-- | Cached widgets operate with widgets in the Identity monad, but they may perform IO using the execute instance
-- of the monad m, which is usually the IO monad. execute basically \"sanctifies\" the use of unsafePerformIO for a transient purpose
-- such is caching. This is defined in "Data.TCache.Memoization". The programmer can create his
-- own instance for his monad.
--
-- With `cachedWidget` it is possible to cache the rendering of a widget as a ByteString (maintaining type safety)
--, permanently or for a certain time. this is very useful for complex widgets that present information. Specially it they must access to databases.
--
-- @
-- import MFlow.Wai.XHtm.All
-- import Some.Time.Library
-- addMessageFlows [(noscript, time)]
-- main= run 80 waiMessageFlow
-- time=do  ask $ cachedWidget \"time\" 5
--            $ wlink () bold << \"the time is \" ++ show (execute giveTheTime) ++ \" click here\"
--          time
-- @
--
-- this pseudocode would update the time every 5 seconds. The execution of the IO computation
-- giveTheTime must be executed inside the cached widget to avoid unnecesary IO executions.
--
-- NOTE: cached widgets are shared by all users
cachedWidget ::(MonadIO m,Typeable view
         , FormInput view, Typeable a,  Executable m )
        => String  -- ^ The key of the cached object for the retrieval
        -> Int     -- ^ Timeout of the caching. Zero means sessionwide
        -> View view Identity a   -- ^ The cached widget, in the Identity monad
        -> View view m a          -- ^ The cached result
cachedWidget key t mf = View .  StateT $ \s -> do

        let((FormElm  form _), sec)= execute $ cachedByKey key t $ proc mf s{mfCached=True}
        let((FormElm  _ mx2), s2)  = execute $ runStateT  ( runView mf)    s{mfSeqCache= sec,mfCached=True}
        let s''=  s{inSync = inSync s2
                   ,mfRequirements=mfRequirements s2
                   ,mfSeqCache= mfSeqCache s + mfSeqCache s2 - sec}
        return $ (mfSeqCache s'') `seq`  ((FormElm form mx2), s'')
        -- !> ("enter: "++show (mfSeqCache s) ++" exit: "++ show ( mfSeqCache s2))
        where
        proc mf s= runStateT (runView mf) s >>= \(r,_) ->mfSeqCache s `seq` return (r,mfSeqCache s )

-- | A shorter name for `cachedWidget`
wcached ::(MonadIO m,Typeable view
         , FormInput view, Typeable a,  Executable m )
        => String  -- ^ The key of the cached object for the retrieval
        -> Int     -- ^ Timeout of the caching. Zero means sessionwide
        -> View view Identity a   -- ^ The cached widget, in the Identity monad
        -> View view m a          -- ^ The cached result
wcached= cachedWidget

-- | Unlike `cachedWidget`, which cache the rendering but not the user response, @wfreeze@
-- cache also the user response. This is useful for pseudo-widgets which just show information
-- while the controls are in other non freezed widgets. A freezed widget ever return the first user response
-- It is faster than `cachedWidget`.
-- It is not restricted to the Identity monad.
--
-- NOTE: cached widgets are shared by all users
wfreeze ::(MonadIO m,Typeable view
         , FormInput view, Typeable a,  Executable m )
        => String  -- ^ The key of the cached object for the retrieval
        -> Int     -- ^ Timeout of the caching. Zero means sessionwide
        -> View view m a   -- ^ The cached widget
        -> View view m a          -- ^ The cached result
wfreeze key t mf = View .  StateT $ \s -> do
        ((FormElm  f mx), req,seq,ajax) <- cachedByKey key t $ proc mf s{mfCached=True}
        return ((FormElm  f mx), s{mfRequirements=req,mfSeqCache= seq,mfAjax=ajax})
        where
        proc mf s= do
          (r,s) <- runStateT (runView mf) s
          return (r,mfRequirements s, mfSeqCache s,mfAjax s)

--
---- | FormLet class
--class (Functor m, MonadIO m) => FormLet  a  m view where
--   digest :: Maybe a
--          -> View view m a

--wrender
--  :: Widget a1 a m v => a1 -> StateT (MFlowState v) m ([v], Maybe a)
--
--wrender x =do
--         (FormElm frm x) <-  runView (widget x)
--         return (frm, x)

-- Minimal definition: either (wrender and wget) or widget
--class (Functor m, MonadIO m) => Widget  a b m view |  a -> b view where
--   wrender :: a -> WState view m [view]
--   wrender x =do
--         (FormElm frm (_ :: Maybe b)) <-  runView (widget x)
--         return frm
--   wget :: a -> WState view m (Maybe b)
--   wget x=  runView (widget x) >>= \(FormElm _ mx) -> return mx

--   widget :: a  -> View view m b
--   widget x = View $  do
--       form <- wrender x  
--       got  <- wget x  
--       return $ FormElm form got


--instance FormLet  a m view => Widget (Maybe a) a m view  where
--   widget = digest

{- | Execute the Flow, in the @FlowM view m@ monad. It is used as parameter of `hackMessageFlow`
`waiMessageFlow` or `addMessageFlows`

The flow is executed in a loop. When the flow is finished, it is started again

@main= do
   addMessageFlows [(\"noscript\",transient $ runFlow mainf)]
   forkIO . run 80 $ waiMessageFlow
   adminLoop
@
-}
runFlow :: (FormInput view, Monad m)
        => FlowM view m () -> Token -> m () 
runFlow  f t=
  loop (runFlowOnce1  f) t --evalStateT (runBackT . runFlowM $ breturn() >>  f)  mFlowState0{mfToken=t,mfEnv= tenv t}  >> return ()  -- >> return ()
  where
  loop f t= f t >>= \t ->  loop  f t


-- | Clears the environment
clearEnv :: MonadState (MFlowState view) m =>  m ()
clearEnv= do
  st <- get
  put st{ mfEnv= []}

runFlowOnce :: (FormInput view,  Monad m)
        => FlowM view m () -> Token -> m ()
runFlowOnce f t= runFlowOnce1 f t >> return ()

runFlowOnce1  f t =
  evalStateT (runBackT . runFlowM $  (clearEnv >> breturn ()) >>  f >> getToken)  mFlowState0{mfToken=t,mfEnv= tenv t} >>= return . fromFailBack   -- >> return ()


  -- to restart the flow in case of going back before the first page of the flow


-- | Run a persistent flow inside the current flow. It is identified by the procedure and
-- the string identifier.
-- unlike the normal flows, that are infinite loops, runFlowIn executes a finite flow
-- once executed, in subsequent executions the flow will return the stored result
-- without asking again. This is useful for asking/storing/retrieving user defined configurations.
runFlowIn
  :: (MonadIO m,
      FormInput view)
  => String
  -> FlowM  view  (Workflow IO)  b
  -> FlowM view m b
runFlowIn wf f= do
  t <- gets mfToken
  FlowM .  BackT $ liftIO $ WF.exec1nc wf $ runFlow1 f t

  where
  runFlow1 f t=   evalStateT (runBackT . runFlowM $ f)  mFlowState0{mfToken=t,mfEnv= tenv t}  -- >>= return . fromFailBack  -- >> return ()

-- | to unlift a FlowM computation. useful for executing the configuration generated by runFLowIn
-- outside of a web application
runFlowConf :: (FormInput view, MonadIO m) 
        => FlowM view m a ->  m a  
runFlowConf  f = do
  q  <- liftIO newEmptyMVar  -- `debug` (i++w++u)
  qr <- liftIO newEmptyMVar
  let t=  Token "" "" "" [] q  qr
  evalStateT (runBackT . runFlowM $   f )  mFlowState0{mfToken=t} >>= return . fromFailBack   -- >> return ()



step
  :: (Serialize a,
      Typeable view,
      FormInput view,
      MonadIO m,
      Typeable a) =>
      FlowM view m a
      -> FlowM view (Workflow m) a
step f= do
   s <- get
   flowM $ BackT $ do
        (r,s') <-  lift . WF.step $ runStateT (runBackT $ runFlowM f) s
        -- when recovery of a workflow, the MFlow state is not considered
        when( mfSequence s' >0) $ put s'
        return r

--stepWFRef
--  :: (Serialize a,
--      Typeable view,
--      FormInput view,
--      MonadIO m,
--      Typeable a) =>
--      FlowM view m a
--      -> FlowM view (Workflow m) (WFRef (FailBack a),a)
--stepWFRef f= do
--   s <- get
--   flowM $ BackT $ do
--        (r,s') <-  lift . WF.stepWFRef $ runStateT (runBackT $ runFlowM f) s
--        -- when recovery of a workflow, the MFlow state is not considered
--        when( mfSequence s' >0) $ put s'
--        return r

--step f= do
--   s <- get
--   flowM $ BackT $ do
--        (r,s') <-   do
--               (br,s') <- runStateT (runBackT $ runFlowM f) s
--               case br of
--                 NoBack r    -> WF.step $ return  r
--                 BackPoint r -> WF.step $ return  r
--                 GoBack      ->  undoStep
--        -- when recovery of a workflow, the MFlow state is not considered
--        when( mfSequence s' >0) $ put s'
--        return r



--stepDebug
--  :: (Serialize a,
--      Typeable view,
--      FormInput view,
--      Monoid view,
--      MonadIO m,
--      Typeable a) =>
--      FlowM view m a
--      -> FlowM view (Workflow m) a
--stepDebug f= BackT  $ do
--      s <- get
--      (r, s') <- lift $ do
--              (r',stat)<- do
--                     rec <- isInRecover
--                     case rec of
--                          True ->do (r',  s'') <- getStep 0
--                                    return (r',s{mfEnv= mfEnv (s'' `asTypeOf`s)})
--                          False -> return (undefined,s)
--              (r'', s''') <- WF.stepDebug  $ runStateT  (runBackT f) stat >>= \(r,s)-> return (r, s)
--              return $ (r'' `asTypeOf` r', s''' )
--     put s'
--     return r



--getParam1 :: (Monad m, MonadState (MFlowState v) m, Typeable a, Read a, FormInput v)
--          => String -> Params -> [v] ->  m (FormElm v a)
--getParam1 par req form=  r
-- where
-- r= case lookup  par req of
--    Just x -> do
--        modify $ \s -> s{inSync= True}
--        maybeRead x                        -- !> x
--    Nothing  -> return $ FormElm form Nothing
-- getType ::  m (FormElm v a) -> a
-- getType= undefined
-- x= getType r
-- maybeRead str= do
--   if typeOf x == (typeOf  ( undefined :: String))
--         then return . FormElm form . Just  $ unsafeCoerce str
--         else case readsPrec 0 $ str of
--              [(x,"")] ->  return . FormElm form  $ Just x
--              _ -> do
--
--                   let err= inred . fromStr $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
--                   return $ FormElm  (form++[err]) Nothing

data ParamResult v a= NoParam | NotValidated String v | Validated a deriving (Read, Show)

getParam1 :: (Monad m, MonadState (MFlowState v) m, Typeable a, Read a, FormInput v)
          => String -> Params ->  m (ParamResult v a)
getParam1 par req =  r
 where
 r= case lookup  par req of
    Just x -> do
        modify $ \s -> s{inSync= True}
        maybeRead x                        -- !> x
    Nothing  -> return  NoParam
 getType ::  m (ParamResult v a) -> a
 getType= undefined
 x= getType r
 maybeRead str= do
   if typeOf x == (typeOf  ( undefined :: String))
         then return . Validated $ unsafeCoerce str
         else case readsPrec 0 $ str of
              [(x,"")] ->  return $ Validated x
              _ -> do

                   let err= inred . fromStr $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ NotValidated str err


---- Requirements


-- | Requirements are javascripts, Stylesheets or server processes (or any instance of the 'Requirement' class) that are included in the
-- Web page or in the server when a widget specifies this. @requires@ is the
-- procedure to be called with the list of requirements.
-- Varios widgets in the page can require the same element, MFlow will install it once.
requires rs =do
    st <- get
    let l = mfRequirements st
--    let rs'= map Requirement rs \\ l
    put st {mfRequirements= l ++ map Requirement rs}



data Requirement= forall a.(Typeable a,Requirements a) => Requirement a deriving Typeable

class Requirements  a where
   installRequirements :: (Monad m,FormInput view) => [a] ->  m view



installAllRequirements ::( Monad m, FormInput view) =>  WState view m view
installAllRequirements= do
 rs <- gets mfRequirements
 installAllRequirements1 mempty rs
 where

 installAllRequirements1 v []= return v
 installAllRequirements1 v rs= do
   let typehead= case head rs of {Requirement r -> typeOf  r}
       (rs',rs'')= partition1 typehead  rs
   v' <- installRequirements2 rs'
   installAllRequirements1 (v `mappend` v') rs''
   where
   installRequirements2 []= return $ fromStrNoEncode ""
   installRequirements2 (Requirement r:rs)= installRequirements $ r:unmap rs
   unmap []=[]
   unmap (Requirement r:rs)= unsafeCoerce r:unmap rs
   partition1 typehead  xs = foldr select  ([],[]) xs
     where
     select  x ~(ts,fs)=
        let typer= case x of Requirement r -> typeOf r
        in if typer== typehead then ( x:ts,fs)
                           else (ts, x:fs)

-- Web requirements ---
loadjsfile filename lcallbacks=
  "var fileref=document.createElement('script');\
  \fileref.setAttribute('type','text/javascript');\
  \fileref.setAttribute('src',\'" ++ filename ++ "\');\
  \document.getElementsByTagName('head')[0].appendChild(fileref);"
  ++ onload
  where
  onload= case lcallbacks of
    [] -> ""
    cs -> "fileref.onload = function() {"++ (concat $ nub cs)++"};"


loadjs content= content


loadcssfile filename=
  "var fileref=document.createElement('link');\
  \fileref.setAttribute('rel', 'stylesheet');\
  \fileref.setAttribute('type', 'text/css');\
  \fileref.setAttribute('href', \'"++filename++"\');\
  \document.getElementsByTagName('head')[0].appendChild(fileref);"


loadcss content=
  "var fileref=document.createElement('link');\
  \fileref.setAttribute('rel', 'stylesheet');\
  \fileref.setAttribute('type', 'text/css');\
  \fileref.innerText=\""++content++"\";\
  \document.getElementsByTagName('head')[0].appendChild(fileref);"






data WebRequirement= JScriptFile
                            String
                            [String]   -- ^ Script URL and the list of scripts to be executed when loaded
                   | CSSFile String    -- ^ a CSS file URL
                   | CSS String        -- ^ a String with a CSS description
                   | JScript String                -- ^ a string with a valid JavaScript
                   | ServerProc (String, Flow)     -- ^ a server procedure
                   deriving(Typeable,Eq,Ord,Show)

instance Eq (String, Flow) where
   (x,_) == (y,_)= x == y

instance Ord (String, Flow) where
   compare(x,_)  (y,_)= compare x y
instance Show (String, Flow) where
   show (x,_)= show x

instance Requirements WebRequirement where
   installRequirements= installWebRequirements



installWebRequirements ::  (Monad m,FormInput view) =>[WebRequirement] -> m view
installWebRequirements rs= do
  let s =  aggregate  $ sort rs

  return $ ftag "script" (fromStrNoEncode  s)
  where
  aggregate  []= ""


  aggregate (r@(JScriptFile f c) : r'@(JScriptFile f' c'):rs)
         | f==f'= aggregate $ JScriptFile f (nub  c++c'):rs
         | otherwise= strRequirement r++aggregate (r':rs)

  aggregate (r:r':rs)
         | r== r' = aggregate $ r:rs
         | otherwise= strRequirement r ++ aggregate (r':rs)

  aggregate (r:rs)= strRequirement r++aggregate rs

  strRequirement  (CSSFile s')         = loadcssfile s'
  strRequirement (CSS s')              = loadcss s'
  strRequirement (JScriptFile s' call) = loadjsfile s' call
  strRequirement (JScript s')          = loadjs s'
  strRequirement (ServerProc  f)= (unsafePerformIO $! addMessageFlows [f]) `seq` ""


--- AJAX ----
ajaxScript=
        "function loadXMLObj()" ++
        "{" ++
        "var xmlhttp;" ++
        "if (window.XMLHttpRequest)" ++
        "{"++
        "  xmlhttp=new XMLHttpRequest();" ++
        "  }" ++
        "else" ++
        "{"++
        "  xmlhttp=new ActiveXObject('Microsoft.XMLHTTP');" ++
        "  }" ++
        "return xmlhttp" ++
        "};" ++

        " xmlhttp= loadXMLObj();" ++
        " noparam= '';"++
        ""++
        "function doServer (servproc,param,param2){" ++
        "   xmlhttp.open('GET',servproc+'?ajax='+param+'&val='+param2,true);" ++
        "   xmlhttp.send();};" ++
        ""++
        "xmlhttp.onreadystatechange=function()" ++
        "  {" ++
        "  if (xmlhttp.readyState== 4 &&  xmlhttp.status==200)" ++
        "    {" ++
        "     eval(xmlhttp.responseText);" ++
        "    }" ++
        "  };" ++
        ""

