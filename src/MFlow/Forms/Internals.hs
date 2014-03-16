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
             -XTemplateHaskell
#-}

module MFlow.Forms.Internals where
import MFlow
import MFlow.Cookies
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Data.ByteString.Lazy.UTF8  as B hiding (length, foldr, take)
import qualified Data.ByteString.UTF8 as SB
import Data.Typeable
import Data.RefSerialize hiding((<|>))
import Data.TCache
import Data.TCache.Memoization
import Data.TCache.DefaultPersistence
import Data.TCache.Memoization
import Data.Dynamic
import qualified Data.Map as M
import Unsafe.Coerce
import Control.Workflow as WF
import Control.Monad.Identity
import Data.List
import System.IO.Unsafe
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Char
import Data.List(stripPrefix)
import Data.Maybe(isJust)
import Control.Concurrent.STM
--import Data.String
--
---- for traces
--

import Control.Exception as CE
import Control.Concurrent 
import Control.Monad.Loc

--import Debug.Trace
--(!>) = flip trace 


data FailBack a = BackPoint a | NoBack a | GoBack   deriving (Show,Typeable)


instance (Serialize a) => Serialize (FailBack a ) where
   showp (BackPoint x)= insertString (fromString iCanFailBack) >> showp x
   showp (NoBack x)   = insertString (fromString noFailBack) >> showp x
   showp GoBack       = insertString (fromString repeatPlease)

   readp = choice [icanFailBackp,repeatPleasep,noFailBackp]
    where
    noFailBackp   = symbol noFailBack >> readp >>= return . NoBack      
    icanFailBackp = symbol iCanFailBack >> readp >>= return . BackPoint 
    repeatPleasep = symbol repeatPlease  >> return  GoBack          

iCanFailBack= "B"
repeatPlease= "G"
noFailBack= "N"

newtype Sup m a = Sup { runSup :: m (FailBack a ) }

class MonadState s m => Supervise s m where
   supBack     :: s -> m ()           -- called before backtracing. state passed is the previous
   supBack = const $ return ()        -- By default the state passed is the last one
   
   supervise ::    m (FailBack a) -> m (FailBack a)
   supervise= id



instance (Supervise s m)=> Monad (Sup  m) where
    fail   _ = Sup . return $ GoBack
    return x = Sup . return $ NoBack x
    x >>= f  = Sup $ loop
     where
     loop = do
        s <- get
        v <- supervise $ runSup x                         -- !> "loop"
        case v of
            NoBack y -> supervise $ runSup (f y)          -- !> "runback"
            BackPoint y  -> do
                 z <- supervise $ runSup (f y)            -- !> "BACK"
                 case z of
                  GoBack  -> supBack s >> loop            -- !> "BACKTRACKING"
                  other   -> return other
            GoBack  ->  return  $ GoBack


fromFailBack (NoBack  x)   = x
fromFailBack (BackPoint  x)= x
toFailBack x= NoBack x


-- | the FlowM monad executes the page navigation. It perform Backtracking when necessary to syncronize
-- when the user press the back button or when the user enter an arbitrary URL. The instruction pointer
-- is moved to the right position within the procedure to handle the request.
--
-- However this is transparent to the programmer, who codify in the style of a console application.
newtype FlowM v m a= FlowM {runFlowM :: FlowMM v m a} deriving (Monad,MonadIO,Functor,MonadState(MFlowState v))
flowM= FlowM
--runFlowM= runView

{-# NOINLINE breturn  #-}

-- | Use this instead of return to return from a computation with ask statements
--
-- This way when the user press the back button, the computation will execute back, to
-- the returned code, according with the user navigation.
breturn :: (Monad m) => a -> FlowM v m a
breturn = flowM . Sup . return . BackPoint           -- !> "breturn"


instance (Supervise s m,MonadIO m) => MonadIO (Sup  m) where
  liftIO f= Sup $ liftIO  f >>= \ x -> return $ NoBack x

instance (Monad m,Functor m) => Functor (Sup m) where
  fmap f g= Sup $ do
     mr <- runSup g
     case mr of
      BackPoint x  -> return . BackPoint $ f x
      NoBack x     -> return . NoBack $ f x
      GoBack       -> return $ GoBack


liftSup f = Sup $ f  >>= \x ->  return $ NoBack x
instance MonadTrans Sup where
  lift f = Sup $ f  >>= \x ->  return $ NoBack x


instance (Supervise s m,MonadState s m) => MonadState s (Sup m) where
   get= lift get                                -- !> "get"
   put= lift . put

type WState view m = StateT (MFlowState view) m
type FlowMM view m=  Sup (WState view m)

data FormElm view a = FormElm [view] (Maybe a) deriving Typeable

instance Serialize a => Serialize (FormElm view a) where
   showp (FormElm _ x)= showp x
   readp= readp >>= \x -> return $ FormElm  [] x

-- | @View v m a@ is a widget (formlet)  with formatting `v`  running the monad `m` (usually `IO`) and which return a value of type `a`
--
-- It has 'Applicative', 'Alternative' and 'Monad' instances.
--
-- Things to know about these instances:
--
--   If the View expression does not validate, ask will present the page again.
--
-- /Alternative instance/: Both alternatives are executed. The rest is as usual
--
-- /Monad Instance/:
--
--  The rendering of each statement is added to the previous. If you want to avoid this, use 'wcallback'
--
--  The execution is stopped when the statement has a formlet-widget that does not validate and
-- return an invalid response (So it will present the page again if no other widget in the expression validates).
--
--  The monadic code is executed from the beginning each time the page is presented or refreshed
--
--  use 'pageFlow' if your page has more than one monadic computation with dynamic behaviour
--
-- use 'pageFlow' to identify each subflow branch of a conditional
--
--  For example:
--
--  > pageFlow "myid" $ do
--  >      r <- formlet1
--  >      liftIO $ ioaction1 r
--  >      s <- formlet2
--  >      liftIO $ ioaction2 s
--  >      case s of
--  >       True  -> pageFlow "idtrue" $ do ....
--  >       False -> paeFlow "idfalse" $ do ...
--  >      ...
--
--  Here if  @formlet2@ do not validate, @ioaction2@ is not executed. But if @formLet1@ validates and the
--  page is refreshed two times (because @formlet2@ has failed, see above),then @ioaction1@ is executed two times.
--  use 'cachedByKey' if you want to avoid repeated IO executions.
newtype View v m a = View { runView :: WState v m (FormElm v a)}


instance  Monad m => Supervise (MFlowState v) (WState v m) where
   supBack st= do     -- the previous state is recovered, with the exception of these fields:
      MFlowState{..} <- get
      put st{ mfEnv= mfEnv,mfToken=mfToken
            , mfPath=mfPath 
            , mfData=mfData
            , mfTrace= mfTrace
            , inSync=False
            , newAsk=False}




instance  MonadLoc (FlowM v IO) where
    withLoc loc f = FlowM . Sup $ do
       withLoc loc $ do
            s <- get
            (r,s') <- lift $ do
                       rs@(r,s') <- runStateT (runSup (runFlowM f) ) s
                                          `CE.catch` (handler1  loc s)
                       case mfTrace s' of
                            []    ->  return rs
                            trace ->  return(r, s'{mfTrace= loc:trace})
            put s'
            return r

       where
       handler1 loc s (e :: SomeException)= do
        case CE.fromException e :: Maybe WFErrors of
           Just e  -> CE.throw e     -- !> ("TROWNF=" ++ show e)
           Nothing ->
             case CE.fromException e :: Maybe AsyncException of
                Just e -> CE.throw e -- !> ("TROWN ASYNCF=" ++ show e)
                Nothing ->
                 return (GoBack, s{mfTrace= [show e]})


--instance (Serialize a,Typeable a, FormInput v) => MonadLoc (FlowM v (Workflow IO)) a where
--    withLoc loc f =  FlowM . Sup $
--       withLoc loc $  do
--            s <- get
--            (r,s') <-  lift . WF.step $ exec1d "jkkjk" ( runStateT (runSup $ runFlowM f) s) `CMT.catch` (handler1  loc s)
--            put s'
--            return r
--
--       where
--       handler1 loc s (e :: SomeException)=
--        return (GoBack, s{mfTrace= Just ["exception: " ++show e]}) 

instance  FormInput v => MonadLoc (View v IO)  where
    withLoc loc f = View $ do
       withLoc loc $ do
            s <- get
            (r,s') <- lift $ do
                       rs@(r,s') <- runStateT (runView f) s
                                             `CE.catch` (handler1  loc s)
                       case mfTrace s' of
                            []     ->  return rs
                            trace  ->  return(r, s'{mfTrace=  loc:trace})
            put s'
            return r

       where
       handler1 loc s (e :: SomeException)= do
        case CE.fromException e :: Maybe WFErrors of
           Just e  -> CE.throw e                -- !> ("TROWN=" ++ show e)
           Nothing ->
             case CE.fromException e :: Maybe AsyncException of
                Just e -> CE.throw e            -- !> ("TROWN ASYNC=" ++ show e)
                Nothing ->
                  return (FormElm [] Nothing, s{mfTrace= [show e]}) -- !> loc







instance (FormInput v,Serialize a)
   => Serialize (a,MFlowState v) where
   showp (x,s)= case mfDebug s of
      False -> showp x
      True  -> showp(x, mfEnv s)
   readp= choice[nodebug, debug]
    where
    nodebug= readp  >>= \x -> return  (x, mFlowState0{mfSequence= -1})
    debug=  do
     (x,env) <- readp
     return  (x,mFlowState0{mfEnv= env,mfSequence= -1})
    

instance Functor (FormElm view ) where
  fmap f (FormElm form x)= FormElm form (fmap f x)

instance  (Monad m,Functor m) => Functor (View view m) where
  fmap f x= View $   fmap (fmap f) $ runView x

  
instance (Functor m, Monad m) => Applicative (View view m) where
  pure a  = View $  return (FormElm [] $ Just a)
  View f <*> View g= View $
                   f >>= \(FormElm form1 k) ->
                   g >>= \(FormElm form2 x) ->
                   return $ FormElm (form1 ++ form2) (k <*> x) 

instance (FormInput view,Functor m, Monad m) => Alternative (View view m) where
  empty= View $ return $ FormElm [] Nothing
  View f <|> View g= View $ do
                   FormElm form1 k <- f
                   s1 <- get
                   FormElm form2 x <- g
                   s2 <- get
                   (mix,hasform) <- controlForms s1 s2 form1 form2
                   when hasform $ put s2{needForm= HasForm}
                   return $ FormElm mix (k <|> x)


instance  (FormInput view, Monad m) => Monad (View view m) where
    View x >>= f = View $ do
                   FormElm form1 mk <- x
                   
                   case mk of
                     Just k  -> do
                        st'' <- get
                        let previousPath= mfPagePath st''
                        let st = st''{ linkMatched = False
                                     , mfPagePath= mfPagePath st'' ++ mfPendingPath st''}
                        put st

                        
                        FormElm form2 mk <- runView $ f k
                        st' <- get
                        (mix, hasform) <- controlForms st st' form1 form2
                        if hasform then put st'{needForm= HasForm,mfPagePath= previousPath}
                                   else put st'{mfPagePath= previousPath}
                        return $ FormElm mix mk

                     Nothing -> 
                        return $ FormElm form1 Nothing
                        


    return = View .  return . FormElm  [] . Just
--    fail msg= View . return $ FormElm [inRed msg] Nothing


  

instance (FormInput v,Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- beware that both operands must validate to generate a sum
  mempty= return mempty


-- | It is a callback in the view monad. The callback rendering substitutes the widget rendering
-- when the latter is validated, without afecting the rendering of other widgets. This allow
-- the simultaneous execution of different behaviours in different widgets in the
-- same page. The inspiration is the callback primitive in the Seaside Web Framework
-- that allows similar functionality (See <http://www.seaside.st>)
--
-- This is the visible difference with 'waction' callbacks, which execute a
-- a flow in the FlowM monad that takes complete control of the navigation, while wactions are
-- executed whithin the same page.
wcallback
  :: Monad m =>
     View view m a -> (a -> View view m b) -> View view m b
wcallback (View x) f = View $ do
   FormElm form1 mk <- x
   case mk of
     Just k  -> do
       modify $ \st -> st{linkMatched= False, needForm=NoElems} 
       runView (f k)
     Nothing -> return $ FormElm form1 Nothing



clear :: (FormInput v,Monad m) => View v m ()
clear = wcallback (return()) (const $ return()) 







instance MonadTrans (View view) where
  lift f = View $  (lift  f) >>= \x ->  return $ FormElm [] $ Just x

instance MonadTrans (FlowM view) where
  lift f = FlowM $ lift (lift  f) -- >>= \x ->  return x

instance  (FormInput view, Monad m)=> MonadState (MFlowState view) (View view m) where
  get = View $  get >>= \x ->  return $ FormElm [] $ Just x
  put st = View $  put st >>= \x ->  return $ FormElm [] $ Just x

--instance  (Monad m)=> MonadState (MFlowState view) (FlowM view m) where
--  get = FlowM $  get >>= \x ->  return $ FormElm [] $ Just x
--  put st = FlowM $  put st >>= \x ->  return $ FormElm [] $ Just x


instance (FormInput view,MonadIO m) => MonadIO (View view m) where
    liftIO io= let x= liftIO io in x `seq` lift x -- to force liftIO==unsafePerformIO on the Identity monad

-- | Execute the widget in a monad and return the result in another.
changeMonad :: (Monad m, Executable m1)
             => View v m1 a -> View v m a
changeMonad w= View . StateT $ \s ->
    let (r,s')= execute $ runStateT  ( runView w)    s
    in mfSequence s' `seq` return (r,s')

-- | True if the flow is going back (as a result of the back button pressed in the web browser).
--  Usually this check is nos necessary unless conditional code make it necessary
--
-- @menu= do
--       mop <- getGoStraighTo 
--       case mop of
--        Just goop -> goop
--        Nothing -> do
--               r \<- `ask` option1 \<|> option2
--               case r of
--                op1 -> setGoStraighTo (Just goop1) >> goop1
--                op2 -> setGoStraighTo (Just goop2) >> goop2@
--
-- This pseudocode below would execute the ask of the menu once. But the user will never have
-- the possibility to see the menu again. To let him choose other option, the code
-- has to be change to
--
-- @menu= do
--       mop <- getGoStraighTo
--       back <- `goingBack`
--       case (mop,back) of
--        (Just goop,False) -> goop
--        _ -> do
--               r \<- `ask` option1 \<|> option2
--               case r of
--                op1 -> setGoStraighTo (Just goop1) >> goop1
--                op2 -> setGoStraighTo (Just goop2) >> goop2@
--
-- However this is very specialized. Normally the back button detection is not necessary.
-- In a persistent flow (with step) even this default entry option would be completely automatic,
-- since the process would restart at the last page visited.
goingBack :: MonadState (MFlowState view) m => m Bool
goingBack = do
    st <- get
    return $ not (inSync st) && not (newAsk st)

-- | Will prevent the Suprack beyond the point where 'preventGoingBack' is located.
-- If the  user press the back button beyond that point, the flow parameter is executed, usually
-- it is an ask statement with a message. If the flow is not going back, it does nothing. It is a cut in Supracking
--
-- It is useful when an undoable transaction has been commited. For example, after a payment.
--
-- This example show a message when the user go back and press again to pay
--
-- >   ask $ wlink () << b << "press here to pay 100000 $ "
-- >   payIt
-- >   preventGoingBack . ask $   b << "You  paid 10000 $ one time"
-- >                          ++> wlink () << b << " Please press here to complete the proccess"
-- >   ask $ wlink () << b << "OK, press here to go to the menu or press the back button to verify that you can not pay again"
-- >   where
-- >   payIt= liftIO $ print "paying"

preventGoingBack
  :: ( Functor m, MonadIO m,  FormInput v) => FlowM v m () -> FlowM v m ()
preventGoingBack msg= do
   back <- goingBack
   if not back  then breturn()  else do
             breturn()  -- will not go back beyond this
             clearEnv
             modify $ \s -> s{newAsk= True}
             msg


-- | executes the first computation when going forward and the second computation when backtracking.
-- Depending on how the second computation finishes, the flow will  resume forward or backward.
onBacktrack :: Monad m => m a -> FlowM v m a -> FlowM v m a
onBacktrack doit onback= do
  back <- goingBack
  case  back of
    False -> (lift doit) >>= breturn
    True  -> onback

-- | less powerflul version of `onBacktrack`: The second computation simply undo the effect of
-- the first one, and the flow continues backward ever. It can be used as a rollback mechanism in
-- the context of long running transactions.
compensate :: Monad m =>  m a ->  m a -> FlowM v m a
compensate doit undoit= doit `onBacktrack` ( (lift undoit) >> fail "")


--orElse ::  FormInput v => FlowM v IO a -> FlowM v IO a -> FlowM v IO a
--orElse mx my= do
--    s <- get
--    let tk = mfToken s
--    (r,s) <- liftIO $ do
--        ref1 <- atomically $ newTVar Nothing
--        ref2 <- atomically $ newTVar Nothing
--        t1 <- forkIO $ (runFlowOnceReturn s mx tk) >>= atomically . writeTVar  ref1 . Just
--        t2 <- forkIO $ (runFlowOnceReturn s my tk) >>= atomically . writeTVar  ref2 . Just
--        r <- atomically $ readFrom ref1 `Control.Concurrent.STM.orElse` readFrom ref2
--        killThread t1
--        killThread t2
--        flushResponse tk
--        flushRec tk
--        return r
--    put s
--    FlowM . Sup $ return r
--    where
--    readFrom ref = do
--      mr <- readTVar ref
--      case mr of
--        Nothing -> retry
--        Just v  -> return v

type Lang=  String

needForm1 st=  case needForm st of
   HasForm -> False
   HasElems -> True
   NoElems -> False

data NeedForm= HasForm | HasElems | NoElems deriving Show

data MFlowState view= MFlowState{   
   mfSequence       :: Int,
   mfCached         :: Bool,
   newAsk           :: Bool,
   inSync           :: Bool,
   mfLang           :: Lang,
   mfEnv            :: Params,
   needForm         :: NeedForm,
   mfToken          :: Token,
   mfkillTime       :: Int,
   mfSessionTime    :: Integer,
   mfCookies        :: [Cookie],
   mfHttpHeaders    :: [(SB.ByteString,SB.ByteString)],
   mfHeader         :: view -> view,
   mfDebug          :: Bool,
   mfRequirements   :: [Requirement],
   mfData           :: M.Map TypeRep Void,
   mfAjax           :: Maybe (M.Map String Void),
   mfSeqCache       :: Int,
   notSyncInAction  :: Bool,

   -- Link management
   mfPath           :: [String],
   mfPagePath       :: [String],
   mfPrefix         :: String,
--   mfPIndex         :: Int,
   mfPageFlow       :: Bool,
   linkMatched      :: Bool,
   mfPendingPath      :: [String],


   mfAutorefresh   :: Bool,
   mfTrace          :: [String],
   mfClear          :: Bool
   }
   deriving Typeable

type Void = Char

mFlowState0 :: (FormInput view) => MFlowState view
mFlowState0 = MFlowState 0 False  True  True  "en"
                [] NoElems  (error "token of mFlowState0 used")
                0 0 [] [] stdHeader False [] M.empty  Nothing 0 False    [] []  "" False False [] False [] False


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

setSessionData ::  (Typeable a,MonadState (MFlowState view) m) => a -> m ()  
setSessionData  x=
  modify $ \st -> st{mfData= M.insert  (typeOf x ) (unsafeCoerce x) (mfData st)}

delSessionData x=
  modify $ \st -> st{mfData= M.delete  (typeOf x ) (mfData st)}
  
-- | Get the session data of the desired type if there is any.
getSessionData ::  (Typeable a, MonadState (MFlowState view) m) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | Return the session identifier
getSessionId :: MonadState (MFlowState v) m => m String
getSessionId= gets mfToken >>= return . key

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
--
setHeader :: MonadState (MFlowState view) m => (view -> view) ->  m ()
setHeader header= do
  fs <- get
  put fs{mfHeader= header}



-- | Return the current header
getHeader :: ( Monad m) => FlowM view m (view -> view)
getHeader= gets mfHeader

-- | Add another header embedded in the previous one
addHeader new= do
  fhtml <- getHeader
  setHeader $  fhtml . new

-- | Set an HTTP cookie
setCookie :: MonadState (MFlowState view) m
          =>  String  -- ^ name
          -> String  -- ^ value
          -> String  -- ^ path
          -> Maybe Integer  -- ^ Max-Age in seconds. Nothing for a session cookie
          -> m ()
setCookie n v p me=
    modify $ \st -> st{mfCookies= (UnEncryptedCookie
                                   ( SB.fromString n,
                                     SB.fromString v,
                                     SB.fromString p,
                                     fmap (SB.fromString . show) me)):mfCookies st }

setParanoidCookie n v p me = setEncryptedCookie' n v p me paranoidEncryptCookie

setEncryptedCookie n v p me = setEncryptedCookie' n v p me encryptCookie

setEncryptedCookie' n v p me encFunc=
    modify $ \st -> st{mfCookies =
                          (unsafePerformIO $ encFunc
                           ( SB.fromString n,
                             SB.fromString v,
                             SB.fromString p,
                             fmap  (SB.fromString . show) me)):mfCookies st }

-- | Set an HTTP Response header
setHttpHeader :: MonadState (MFlowState view) m
           => SB.ByteString  -- ^ name
          -> SB.ByteString  -- ^ value
          -> m ()
setHttpHeader n v =
    modify $ \st -> st{mfHttpHeaders = nubBy (\ x y -> fst x == fst y) $ (n,v):mfHttpHeaders st }


-- | Set
--  1) the timeout of the flow execution since the last user interaction.
-- Once passed, the flow executes from the begining.
--
-- 2) In persistent flows
-- it set the session state timeout for the flow, that is persistent. If the
-- flow is not persistent, it has no effect.
--
-- As the other state primitives, it can be run in the Flow and in the View monad
--
-- `transient` flows restart anew.
-- persistent flows (that use `step`) restart at the las saved execution point, unless
-- the session time has expired for the user.
setTimeouts :: ( MonadState (MFlowState v) m) => Int -> Integer ->  m ()
setTimeouts kt st= do
 fs <- get
 put fs{ mfkillTime= kt, mfSessionTime= st}


getWFName ::   MonadState (MFlowState view) m =>   m String
getWFName = do
 fs <- get
 return . twfname $ mfToken fs

getCurrentUser ::  MonadState (MFlowState view) m =>  m String
getCurrentUser = do
  st<- gets mfToken
  return $ tuser st

type Name= String
type Type= String
type Value= String
type Checked= Bool
type OnClick= Maybe String

normalize :: (Monad m, FormInput v) => View v m a -> View B.ByteString m a
normalize f=  View .  StateT $ \s ->do
       (FormElm fs mx, s') <-  runStateT  ( runView f) $ unsafeCoerce s
       return  (FormElm (map toByteString fs ) mx,unsafeCoerce s')



-- | Minimal interface for defining the basic form and link elements. The core of MFlow is agnostic
-- about the rendering package used. Every formatting (either HTML or not) used with MFlow must have an
-- instance of this class.
-- See "MFlow.Forms.Blaze.Html for the instance for blaze-html" "MFlow.Forms.XHtml" for the instance
-- for @Text.XHtml@ and MFlow.Forms.HSP for the instance for Haskell Server Pages.
class (Monoid view,Typeable view)   => FormInput view where
    toByteString :: view -> B.ByteString
    toHttpData :: view -> HttpData
    fromStr :: String -> view
    fromStrNoEncode :: String -> view
    ftag :: String -> view  -> view
    inred   :: view -> view
    flink ::  String -> view -> view 
    flink1:: String -> view
    flink1 verb = flink verb (fromStr  verb) 
    finput :: Name -> Type -> Value -> Checked -> OnClick -> view 
    ftextarea :: String -> T.Text -> view
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
-- import MFlow.Wai.Blaze.Html.All
-- import Some.Time.Library
-- addMessageFlows [(noscript, time)]
-- main= run 80 waiMessageFlow
-- time=do  ask $ cachedWidget \"time\" 5
--              $ wlink () b << \"the time is \" ++ show (execute giveTheTime) ++ \" click here\"
--              time
-- @
--
-- this pseudocode would update the time every 5 seconds. The execution of the IO computation
-- giveTheTime must be executed inside the cached widget to avoid unnecesary IO executions.
--
-- NOTE: the rendering of cached widgets are shared by all users
cachedWidget :: (MonadIO m,Typeable view
         , FormInput view, Typeable a,  Executable m )
        => String  -- ^ The key of the cached object for the retrieval
        -> Int     -- ^ Timeout of the caching. Zero means the whole server run
        -> View view Identity a   -- ^ The cached widget, in the Identity monad
        -> View view m a          -- ^ The cached result
cachedWidget key t mf =  View .  StateT $ \s ->  do
        let((FormElm  form _), sec)= execute $! cachedByKey key t $ proc mf s{mfCached=True}
        let((FormElm  _ mx2), s2)  = execute $ runStateT  ( runView mf)    s{mfSeqCache= sec,mfCached=True}
        let s''=  s{inSync = inSync s2
                   ,mfRequirements=mfRequirements s2
                   ,mfPath= mfPath s2
                   ,mfPagePath= mfPagePath s2
                   ,mfPendingPath=mfPendingPath s2  
                   ,needForm= needForm s2
                   ,mfPageFlow= mfPageFlow s2
                   ,mfSeqCache= mfSeqCache s + mfSeqCache s2 - sec}
        return $ (mfSeqCache s'') `seq` form `seq`  ((FormElm form mx2), s'')

        -- !> ("enter: "++show (mfSeqCache s) ++" exit: "++ show ( mfSeqCache s2))
        where
        proc mf s= runStateT (runView mf) s >>= \(r,_) -> mfSeqCache s `seq` return (r,mfSeqCache s )

-- | A shorter name for `cachedWidget`
wcached :: (MonadIO m,Typeable view
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
-- NOTE: the content of freezed widgets are shared by all users
wfreeze :: (MonadIO m,Typeable view
         , FormInput view, Typeable a,  Executable m )
        => String          -- ^ The key of the cached object for the retrieval
        -> Int             -- ^ Timeout of the caching. Zero means sessionwide
        -> View view m a   -- ^ The cached widget
        -> View view m a   -- ^ The cached result
wfreeze key t mf = View .  StateT $ \s -> do
        (FormElm  f mx, req,seq,ajax) <- cachedByKey key t $ proc mf s{mfCached=True}
        return ((FormElm f mx), s{mfRequirements=req ,mfSeqCache= seq,mfAjax=ajax})
        where
        proc mf s= do
          (r,s) <- runStateT (runView mf) s
          return (r,mfRequirements s, mfSeqCache s, mfAjax s)



{- | Execute the Flow, in the @FlowM view m@ monad. It is used as parameter of `hackMessageFlow`
`waiMessageFlow` or `addMessageFlows`

The flow is executed in a loop. When the flow is finished, it is started again

@main= do
   addMessageFlows [(\"noscript\",transient $ runFlow mainf)]
   forkIO . run 80 $ waiMessageFlow
   adminLoop
@
-}
runFlow :: (FormInput view, MonadIO m)
        => FlowM view m () -> Token -> m () 
runFlow  f t=
  loop (startState t) f   t 
  where
  loop  s f t = do
    (mt,s) <- runFlowOnce2 s f  
    let t'= fromFailBack mt
    let t''= t'{tpath=[twfname t']}
    liftIO $ do
       flushRec t'' 
       sendToMF t'' t''    -- !> "SEND"
    let s'= case mfSequence s of
             -1 -> s --in recovery
             _  -> s{mfPath=[twfname t],mfPagePath=[twfname t],mfEnv=[]}
    loop   s' f t''{tpath=[]}           -- !> "LOOPAGAIN"



runFlowOnce :: (MonadIO m, FormInput view)
        => FlowM view m () -> Token -> m ()
runFlowOnce f t= runFlowOnce1 f t  >> return ()

runFlowOnce1 f t  = runFlowOnce2 (startState t)  f

startState t= mFlowState0{mfToken=t
                   ,mfPath= tpath t
                   ,mfEnv= tenv t
                   ,mfPagePath=[twfname t]}  

runFlowOnce2 s f  =
  runStateT (runSup . runFlowM $ do
        backInit
        f  
        getToken) s
        

  where
  backInit= do
     s <- get                     --   !> "BackInit"
     case mfTrace s of
       [] -> do
         modify $ \s -> s{ newAsk= True}
         breturn ()
         
       tr ->  error $ disp tr
     where
     disp tr= "TRACE (error in the last line):\n\n" ++(concat $ intersperse "\n" tr)
  -- to restart the flow in case of going back before the first page of the flow

runFlowOnceReturn
  ::   FormInput v => MFlowState v -> FlowM v m a -> Token -> m (FailBack a, MFlowState v)
runFlowOnceReturn  s f t =
  runStateT (runSup $ runFlowM f) (startState t)
        


-- | Run a persistent flow inside the current flow. It is identified by the procedure and
-- the string identifier.
-- unlike the normal flows, that run within infinite loops, runFlowIn executes once.
-- In subsequent executions, the flow will get the intermediate responses from te log
-- and will return the result without asking again.
-- This is useful for asking once, storing in the log and subsequently retrieving user
-- defined configurations by means of persistent flows with web formularies.
runFlowIn
  :: (MonadIO m,
      FormInput view)
  => String
  -> FlowM  view  (Workflow IO)  b
  -> FlowM view m b
runFlowIn wf f= FlowM . Sup $ do
      st <- get     
      let t = mfToken st
      (r,st') <- liftIO $ exec1nc wf $ runFlow1 st f t
      put st{mfPath= mfPath st'}
      case r of
        GoBack ->  delWF  wf ()
      return r

  where
  runFlow1 st f t= runStateT (runSup . runFlowM $ f) st


-- | to unlift a FlowM computation. useful for executing the configuration generated by runFLowIn
-- outside of the web flow (FlowM) monad
runFlowConf :: (FormInput view, MonadIO m) => FlowM view m a ->  m a
runFlowConf  f = do
  q  <- liftIO newEmptyMVar  -- `debug` (i++w++u)
  qr <- liftIO newEmptyMVar
  let t=  Token "" "" "" [] [] q  qr
  evalStateT (runSup . runFlowM $   f )  mFlowState0{mfToken=t} >>= return . fromFailBack   -- >> return ()



-- | Clears the environment
clearEnv :: MonadState (MFlowState view) m =>  m ()
clearEnv= do
  st <- get
  put st{ mfEnv= []}


-- | stores the result of the flow in a  persistent log. When restarted, it get the result
-- from the log and it does not execute it again. When no results are in the log, the computation
-- is executed. It is equivalent to 'Control.Workflow.step' but in the FlowM monad.
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
   flowM $ Sup $ do
        (r,s') <- lift . WF.step $ runStateT (runSup $ runFlowM f) s

        -- when recovery of a workflow, the MFlow state is not considered
        when( mfSequence s' /= -1) $ put s'  -- !> (show $ mfSequence s') -- else put  s{newAsk=True}
        return r

-- | to execute transient flows as if they were persistent
-- it can be used instead of step, but it does  log nothing.
-- Thus, it is faster and convenient when no session state must be stored beyond the lifespan of
-- the server process.
--
-- > transient $ runFlow f === runFlow $ transientNav f
transientNav
  :: (Serialize a,
      Typeable view,
      FormInput view,
      Typeable a) =>
      FlowM view IO a
      -> FlowM view (Workflow IO) a
transientNav f= do
   s <- get
   flowM $ Sup $ do
        (r,s') <-  lift . unsafeIOtoWF $ runStateT (runSup $ runFlowM f) s
        put s'
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
--   flowM $ Sup $ do
--        (r,s') <-  lift . WF.stepWFRef $ runStateT (runSup $ runFlowM f) s
--        -- when recovery of a workflow, the MFlow state is not considered
--        when( mfSequence s' >0) $ put s'
--        return r

--step f= do
--   s <- get
--   flowM $ Sup $ do
--        (r,s') <-   do
--               (br,s') <- runStateT (runSup $ runFlowM f) s
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
--stepDebug f= Sup  $ do
--      s <- get
--      (r, s') <- lift $ do
--              (r',stat)<- do
--                     rec <- isInRecover
--                     case rec of
--                          True ->do (r',  s'') <- getStep 0
--                                    return (r',s{mfEnv= mfEnv (s'' `asTypeOf`s)})
--                          False -> return (undefined,s)
--              (r'', s''') <- WF.stepDebug  $ runStateT  (runSup f) stat >>= \(r,s)-> return (r, s)
--              return $ (r'' `asTypeOf` r', s''' )
--     put s'
--     return r



data ParamResult v a= NoParam | NotValidated String v | Validated a deriving (Read, Show)

valToMaybe (Validated x)= Just x
valToMaybe _= Nothing

isValidated (Validated x)= True
isValidated _= False

fromValidated (Validated x)= x
fromValidated NoParam= error $ "fromValidated : NoParam"
fromValidated (NotValidated s err)= error $ "fromValidated: NotValidated "++ s



getParam1 :: (Monad m, MonadState (MFlowState v) m, Typeable a, Read a, FormInput v)
          => String -> Params ->  m (ParamResult v a)
getParam1 par req =   case lookup  par req of
    Just x -> readParam x  
    Nothing  -> return  NoParam

-- Read a segment in the REST path. if it does not match with the type requested
-- or if there is no remaining segment, it returns Nothing
getRestParam :: (Read a, Typeable a,Monad m,Functor m,  MonadState (MFlowState v) m, FormInput v) => m (Maybe a)
getRestParam= do
  st <- get
  let lpath  = mfPath st
  
  if  linkMatched st
   then return Nothing          
   else case  stripPrefix (mfPagePath st) lpath  of
     Nothing -> return Nothing
     Just [] -> return Nothing
     Just xs -> do
          let name = head xs
          modify $ \s -> s{inSync= True
                         ,linkMatched= True
                         ,mfPendingPath= mfPendingPath s++[name] } 
          fmap valToMaybe $ readParam name


-- | return the value of a post or get param in the form ?param=value&param2=value2...
getKeyValueParam par= do
  st <- get
  r <- getParam1 par $ mfEnv st
  return $ valToMaybe r
  
readParam :: (Monad m, MonadState (MFlowState v) m, Typeable a, Read a, FormInput v)
           => String -> m (ParamResult v a)
readParam x1 = r
 where
 r= do
     modify $ \s -> s{inSync= True}
     maybeRead x1
      
 getType ::  m (ParamResult v a) -> a
 getType= undefined
 x= getType r
 maybeRead str= do
   let typeofx = typeOf x
   if typeofx == typeOf  ( undefined :: String)   then
           return . Validated $ unsafeCoerce str
    else if typeofx == typeOf (undefined :: T.Text) then
           return . Validated . unsafeCoerce  $ T.pack str
    else case readsPrec 0 $ str of
              [(x,"")] ->  return $ Validated x
              _ -> do
                   let err= inred . fromStr $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ NotValidated str err

---- Requirements


-- | Requirements are javascripts, Stylesheets or server processes (or any instance of the 'Requirement' class) that are included in the
-- Web page or in the server when a widget specifies this. @requires@ is the
-- procedure to be called with the list of requirements.
-- Various widgets in the page can require the same element, MFlow will install it once.
requires rs =do
    st <- get 
    let l = mfRequirements st
--    let rs'= map Requirement rs \\ l
    put st {mfRequirements= l ++ map Requirement rs}



data Requirement= forall a.(Show a,Typeable a,Requirements a) => Requirement a deriving Typeable

class Requirements  a where
   installRequirements :: (Monad m,FormInput view) => [a] ->  m view

instance Show Requirement where
   show (Requirement a)= show a ++ "\n"

installAllRequirements :: ( Monad m, FormInput view) =>  WState view m view
installAllRequirements= do
 rs <- gets mfRequirements
 installAllRequirements1 mempty rs 
 where

 installAllRequirements1 v []= return v
 installAllRequirements1 v rs= do
   let typehead= case head rs  of {Requirement r -> typeOf  r}
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
  let s =  jsRequirements $ sort rs

  return $ ftag "script" (fromStrNoEncode  s)


jsRequirements  []= ""


jsRequirements (r@(JScriptFile f c) : r'@(JScriptFile f' c'):rs)
         | f==f' = jsRequirements $ JScriptFile f (nub $ c++c'):rs
         | otherwise= strRequirement r ++ jsRequirements (r':rs)

jsRequirements (r:r':rs)
         | r== r' = jsRequirements $ r:rs
         | otherwise= strRequirement r ++ jsRequirements (r':rs)

jsRequirements (r:rs)= strRequirement r++jsRequirements rs
  
strRequirement (CSSFile s')          = loadcssfile s'
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

formPrefix   st form anchored= do
     let verb = twfname $ mfToken st
         path  = currentPath st 
     (anchor,anchorf)
           <- case anchored of
               True  -> do
                        anchor <- genNewId
                        return ('#':anchor, (ftag "a") mempty  `attrs` [("name",anchor)])
               False -> return (mempty,mempty)
     return $ formAction (path ++ anchor ) $  mconcat ( anchorf:form)  -- !> anchor

-- | insert a form tag if the widget has form input fields. If not, it does nothing
insertForm w=View $ do
    FormElm forms mx <- runView w
    st <- get
    cont <- case needForm1 st of
              True ->  do
                       frm <- formPrefix  st forms False
                       put st{needForm= HasForm}
                       return   frm
              _    ->  return $ mconcat  forms
    
    return $ FormElm [cont] mx

-- isert a form tag if necessary when two pieces of HTML have to mix as a result of >>= >> <|>  or <+> operators
controlForms :: (FormInput v, MonadState (MFlowState v) m)
    => MFlowState v -> MFlowState v -> [v] -> [v] -> m ([v],Bool)
controlForms s1 s2 v1 v2= case (needForm s1, needForm s2) of
--    (HasForm,HasElems) -> do
--       v2' <- formPrefix s2 v2 True
--       return (v1 ++ [v2'], True)
    (HasElems, HasForm) -> do
       v1' <- formPrefix s1 v1 True
       return ([v1'] ++ v2 , True)

    _ -> return (v1 ++ v2, False)

currentPath  st=  concat ['/':v| v <- mfPagePath st]

-- | Generate a new string. Useful for creating tag identifiers and other attributes.
--
-- if the page is refreshed, the identifiers generated are the same.
genNewId :: MonadState (MFlowState view) m =>  m String
genNewId=  do
  st <- get
  case mfCached st of
    False -> do
      let n= mfSequence st
          prefseq=  mfPrefix st
      put $ st{mfSequence= n+1}

      return $ 'p':show n++prefseq  
    True  -> do
      let n = mfSeqCache st
      put $ st{mfSeqCache=n+1}
      return $  'c' : (show n)

-- | get the next ideitifier that will be created by genNewId
getNextId :: MonadState (MFlowState view) m =>  m String
getNextId=  do
  st <- get
  case mfCached st of
    False -> do
      let n= mfSequence st
          prefseq=  mfPrefix st
      return $ 'p':show n++prefseq
    True  -> do
      let n = mfSeqCache st
      return $  'c' : (show n)

