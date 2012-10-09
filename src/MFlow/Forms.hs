{-# OPTIONS  -XDeriveDataTypeable
             -XUndecidableInstances
             -XExistentialQuantification
             -XMultiParamTypeClasses
             -XTypeSynonymInstances
             -XFlexibleInstances
             -XScopedTypeVariables
             -XFunctionalDependencies
             -XFlexibleContexts
             -XRecordWildCards
             -XIncoherentInstances
             -XTypeFamilies
             -XTypeOperators
             -XOverloadedStrings
#-}

{- | This module implement  stateful processes (flows) that are optionally persistent.
This means that they automatically store and recover his execution state. They are executed by the MFlow app server.
defined in the "MFlow" module.

These processses interact with the user trough user interfaces made of widgets (see below) that return back statically typed responses to
the calling process. Because flows are stateful, not request-response, the code is more understandable, because
all the flow of request and responses is coded by the programmer in a single function. Allthoug
single request-response flows and callbacks are possible.

This module is abstract with respect to the formatting (here referred with the type variable @view@) . For an
instantiation for "Text.XHtml"  import "MFlow.Forms.XHtml", "MFlow.Hack.XHtml.All"  or "MFlow.Wai.XHtml.All" .
To use Haskell Server Pages import "MFlow.Forms.HSP". However the functionalities are documented here.

`ask` is the only method for user interaction. It run in the @MFlow view m@ monad, with @m@ the monad chosen by the user, usually IO.
It send user interfaces (in the @View view m@ monad) and return statically
typed responses. The user interface definitions are  based on a extension of
formLets (<http://www.haskell.org/haskellwiki/Formlets>) with the addition of caching, links, formatting, attributes,
 extra combinators, callbaks and modifiers.
The interaction with the user is  stateful. In the same computation there may be  many
request-response interactions, in the same way than in the case of a console applications. 

* APPLICATION SERVER

Therefore, session and state management is simple and transparent: it is in the haskell
structures in the scope of the computation. `transient` (normal) procedures have no persistent session state
and `stateless` procedures accept a single request and return a single response.

`MFlow.Forms.step` is a lifting monad transformer that permit persistent server procedures that
remember the execution state even after system shutdowns by using the package workflow (<http://hackage.haskell.org/package/Workflow>) internally.
This state management is transparent. There is no programer interface for session management.

The programmer set the process timeout and the session timeout with `setTimeouts`.
If the procedure has been stopped due to the process timeout or due to a system shutdowm,
the procedure restart in the last state when a request for this procedure arrives
(if the procedure uses the `step` monad transformer)

* WIDGETS

The correctness of the web responses is assured by the use of formLets.
But unlike formLets in its current form, it permits the definition of widgets.
/A widget is a combination of formLets and links within its own formatting template/, all in
the same definition in the same source file, in plain declarative Haskell style.

The formatting is abstract. It has to implement the 'FormInput' class.
There are instances for Text.XHtml ("MFlow.Forms.XHtml"), Haskell Server Pages ("MFlow.Forms.HSP")
and ByteString. So widgets
can use any formatting that is instance of `FormInput`.
It is possible to use more than one format in the same widget.

Links defined with `wlink` are treated the same way than forms. They are type safe and return values
 to the same flow of execution.
It is posssible to combine links and forms in the same widget by using applicative combinators  but also
additional applicative combinators like  \<+> !*> , |*|. Widgets are also monoids, so they can
be combined as such.

* NEW IN THIS RELEASE:

[@Back Button@] This is probably the first implementation of an stateful Web framework that works well with the back button, thanks
to monad magic. (See <http://haskell-web.blogspot.com.es/2012/03//failback-monad.html>)


[@Cached widgets@] with `cachedWidget` it is possible to cache the rendering of a widget as a ByteString (maintaining type safety)
, the caching can be permanent or for a certain time. this is very useful for complex widgets that present information. Specially if
the widget content comes from a database and it is shared by all users.


[@Callbacks@] `waction` add a callback to a widget. It is executed when its input is validated.
The callback may initate a flow of interactions with the user or simply executes an internal computation.
Callbacks are necessary for the creation of abstract container
widgets that may not know the behaviour of its content. with callbacks, the widget manages its content as  black boxes.


[@Modifiers@] `wmodify` change the visualization and result returned by the widget. For example it may hide a
login form and substitute it by the username if already logged.

Example:

@ ask $ wform userloginform \``validate`\` valdateProc \``waction`\` loginProc \``wmodify`\` hideIfLogged@


[@attributes for formLet elements@] it is not only possible to add Html formatting, but also to add atributes to a formlet element.
This example has three formLet elements with the attribute "size" added, and a string prepended to the second password box.

> userFormLine=
>        (User <$> getString (Just "enter user")                  <! [("size","5")]
>              <*> getPassword                                    <! [("size","5")]
>              <** submitButton "login")
>              <+> (fromString "  password again" ++> getPassword  <! [("size","5")]
>              <*  submitButton "register")


[@ByteString normalization and hetereogeneous formatting@] For caching the rendering of widgets at the ByteString level, and to permit many formatring styles
in the same page, there are operators that combine different formats which are converted to ByteStrings.
For example the header and footer may be coded in XML, while the formlets may be formatted using Text.XHtml.


[@AJAX@] See "MFlow.Forms.Ajax"


[@File Server@] With file caching. See "MFlow.FileServer"

This is a complete example, that can be run with runghc, which show some of these features:

> {-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
> module Main where
> import MFlow.Wai.XHtml.All
> import Data.TCache
> import Control.Monad.Trans
> import Data.Typeable
> import Control.Concurrent
> import Control.Exception as E
> import qualified Data.ByteString.Char8 as SB
> import qualified Data.Vector as V
> import Data.Maybe
>
> data Ops= Ints | Strings | Actions | Ajax | Opt deriving(Typeable,Read, Show)
> main= do
>    setFilesPath ""
>    addFileServerWF
>    addMessageFlows [(""  ,transient $ runFlow mainf)
>                    ,("shop"    ,runFlow shopCart)]
>    forkIO $ run 80 waiMessageFlow
>    adminLoop
>
> stdheader c= p << "you can press the back button to go to the menu"+++ c
>
> mainf=   do
>        setHeader stdheader
>        r <- ask $   wlink Ints (bold << "increase an Int")
>                <|>  br ++> wlink Strings (bold << "increase a String")
>                <|>  br ++> wlink Actions (bold << "Example of a string widget with an action")
>                <|>  br ++> wlink Ajax (bold << "Simple AJAX example")
>                <|>  br ++> wlink Opt (bold << "select options")
>                <++ (br +++ linkShop) -- this is an ordinary XHtml link
>
>        case r of
>          Ints    ->  clickn 0
>          Strings ->  clicks "1"
>          Actions ->  actions 1
>          Ajax    ->  ajaxsample
>          Opt     ->  options
>        mainf
>     where
>     linkShop= toHtml $ hotlink  "shop" << "shopping"
>
> options= do
>    r <- ask $ getSelect (setOption "blue" (bold << "blue")   <|>
>                          setSelectedOption "Red"  (bold << "red")  ) <! dosummit
>    ask $ p << (r ++ " selected") ++> wlink () (p<< " menu")
>    breturn()
>    where
>    dosummit= [("onchange","this.form.submit()")]
>
> clickn (n :: Int)= do
>    setHeader stdheader
>    r <- ask $  wlink "menu" (p << "menu")
>            |+| getInt (Just n) <* submitButton "submit"
>    case r of
>     (Just _,_) -> breturn ()
>     (_, Just n') -> clickn $ n'+1
>
>
> clicks s= do
>    setHeader stdheader
>    s' <- ask $ (getString (Just s)
>              <* submitButton "submit")
>              `validate` (\s -> return $ if length s   > 5 then Just "length must be < 5" else Nothing )
>    clicks $ s'++ "1"
>
>
> ajaxheader html= thehtml << ajaxHead << p << "click the box" +++ html
>
> ajaxsample= do
>    setHeader ajaxheader
>    ajaxc <- ajaxCommand "document.getElementById('text1').value"
>                         (\n ->  return $ "document.getElementById('text1').value='"++show(read  n +1)++"'")
>    ask $ (getInt (Just 0) <! [("id","text1"),("onclick", ajaxc)])
>    breturn()
>
> actions n=do
>   ask $ wlink () (p << "exit from action")
>      <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )
>   breturn ()
>
> -- A persistent flow  (uses step). The process is killed after 10 seconds of inactivity
> -- but it is restarted automatically. if you restart the program, it remember the shopping cart
> -- defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.
> shopCart  = do
>    setTimeouts 10 0
>    shopCart1 (V.fromList [0,0,0:: Int])
>    where
>    shopCart1 cart=  do
>      i <- step . ask $
>              table ! [border 1,thestyle "width:20%;margin-left:auto;margin-right:auto"]
>              <<< caption << "choose an item"
>              ++> thead << tr << concatHtml[ th << bold << "item", th << bold << "times chosen"]
>              ++> (tbody
>                   <<<  tr ! [rowspan 2] << td << linkHome
>                   ++> (tr <<< td <<< wlink  0 (bold <<"iphone") <++  td << ( bold << show ( cart V.! 0))
>                   <|>  tr <<< td <<< wlink  1 (bold <<"ipad")   <++  td << ( bold << show ( cart V.! 1))
>                   <|>  tr <<< td <<< wlink  2 (bold <<"ipod")   <++  td << ( bold << show ( cart V.! 2)))
>                   <++  tr << td << linkHome
>                   )
>
>      let newCart= cart V.// [(i, cart V.! i + 1 )]
>      shopCart1 newCart
>      where
>      linkHome= (toHtml $ hotlink  noScript << bold << "home")

-}

module MFlow.Forms(

-- * Basic definitions 
FormLet(..), 
FlowM,View, FormInput(..)

-- * Users 
,userRegister, userValidate, isLogged, User(userName), setAdminUser, getAdminName
,getCurrentUser,getUserSimple, getUser, userFormLine, userLogin, userWidget,
-- * User interaction 
ask, clearEnv, 
-- * formLets 
-- | they mimic the HTML form elements.
-- It is possible to modify their attributes with the `<!` operator.
-- They are combined with the widget combinators.
-- formatting can be added with the formatting combinators.
-- modifiers change their presentation and behaviour
getString,getInt,getInteger, getTextBox
,getMultilineText,getBool,getSelect, setOption,setSelectedOption, getPassword,
getRadio, getRadioActive, getCheckBoxes, setCheckBox,
submitButton,resetButton, wlink, wform,
 firstOf,
-- * FormLet modifiers
validate, noWidget, wrender, waction, wmodify,

-- * Caching widgets
cachedWidget,
-- * Widget combinators
(<+>),(|*>),(|+|), (**>),(<**),(<|>),(<*),(<$>),(<*>),

-- * Normalized (convert to ByteString) widget combinators
-- | these dot operators are indentical to the non dot operators, with the addition of the conversion of the arguments to lazy byteStrings
--
-- The purpose is to combine heterogeneous formats into byteString-formatted widgets that
-- can be cached with `cachedWidget`
(.<+>.), (.|*>.), (.|+|.), (.**>.),(.<**.), (.<|>.),

-- * Formatting combinators
(<<<),(<++),(++>),(<!),

-- * Normalized (convert to ByteString) formatting combinators
-- | some combinators that convert the formatting of their arguments to lazy byteString
(.<<.),(.<++.),(.++>.)

-- * ByteString tags
,btag,bhtml,bbody

-- * Normalization
, flatten, normalize, ToByteString(..)

-- * Running the flow monad
,runFlow,runFlowIn,MFlow.Forms.step, goingBack,breturn

-- * Setting parameters
,setHeader
,getHeader
,setTimeouts

-- * Cookies
,setCookie

)
where
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.Memoization
import MFlow
import MFlow.Cookies
import Data.RefSerialize hiding((<|>))
import Data.ByteString.Lazy.Char8 as B(ByteString,cons,pack,unpack,append,empty,fromChunks) 

import qualified Data.CaseInsensitive as CI
import Data.Typeable
import Data.Monoid
import Control.Monad.State.Strict 
import Control.Monad.Trans.Maybe
import Data.Maybe
import Control.Applicative
import Control.Exception

import Control.Workflow as WF 
import Control.Monad.Identity
import Unsafe.Coerce
import Data.List(intersperse)
import Data.IORef

import System.IO.Unsafe
import Data.Char(isNumber)
import Network.HTTP.Types.Header

import Debug.Trace
(!>)= flip trace


instance Serialize a => Serializable a where
  serialize=  runW . showp
  deserialize=   runR readp

data User= User
            { userName :: String
            , upassword :: String
            } deriving (Read, Show, Typeable)

eUser= User (error1 "username") (error1 "password")

error1 s= error $ s ++ " undefined"

userPrefix= "User/"
instance Indexable User where
   key User{userName= user}= keyUserName user

-- | return  the key name of an user
keyUserName n= userPrefix++n

-- | Register an user/password 
userRegister :: MonadIO m => String -> String  -> m (DBRef User)
userRegister user password  = liftIO . atomically $ newDBRef $ User user password



-- | Authentication against `userRegister`ed users.
-- to be used with `validate`
userValidate :: MonadIO m =>  (UserStr,PasswdStr) -> m (Maybe String)
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
     err= Just "Username or password invalid"

data Config = Config UserStr deriving (Read, Show, Typeable)

keyConfig= "MFlow.Config"
instance Indexable Config where key _= keyConfig
rconf= getDBRef keyConfig

setAdminUser :: MonadIO m => UserStr -> PasswdStr -> m ()
setAdminUser user password= liftIO $  atomically $ do
  newDBRef $ User user password
  writeDBRef rconf $ Config user

getAdminName :: MonadIO m => m UserStr
getAdminName= liftIO $ atomically ( readDBRef rconf `onNothing` error "admin user not set" ) >>= \(Config u) -> return u


--test= runBackT $ do
--        liftRepeat $ print "hola"
--        n2 <- lift $ getLine
--        lift $ print "n3"
--
--        n3  <- lift $  getLine
--        if n3 == "back"
--                   then  fail ""
--                  else lift $ print  $ n2++n3


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

--instance Monad m => Monad (BackT  m) where
--    fail   _ = BackT $ return GoBack
--    return x = BackT . return $ NoBack x
--    x >>= f  = BackT $ loop
--     where
--     loop = do
--      res<- do
--        v <- runBackT x                          -- !> "loop"
--        case v of
--            NoBack y  -> runBackT (f y) >>= return . Right       -- !> "runback"
--            BackPoint y  -> do
--                 z <- runBackT (f y)           -- !> "BACK"
--                 case z of
--                  GoBack  -> return $ Left ()             -- !> "GoBack"
--                  other -> return $ Right other
--            GoBack -> return  $ Right GoBack
--      case res of
--        Left _ -> loop
--        Right r -> return r

instance Monad m => Monad (BackT  m) where
    fail   _ = BackT . return $ GoBack
    return x = BackT . return $ NoBack x
    x >>= f  = BackT $ loop
     where
     loop = do
        v <- runBackT x                          -- !> "loop"
        case v of
            NoBack y  -> runBackT (f y)         -- !> "runback"
            BackPoint y  -> do
                 z <- runBackT (f y)           -- !> "BACK"
                 case z of
                  GoBack   -> loop              -- !> "GoBack"
                  other -> return other
            GoBack  -> return  $ GoBack

--instance Monad m => Monad (BackT  m) where
--    fail   _ = BackT $ return GoBack
--    return x = BackT . return $ NoBack x
--    x >>= f  = BackT $  do
--        v <- runBackT x
--        case v of
--            NoBack y  -> runBackT (f y)
--            BackPoint y  -> loop
--             where
--             loop= do
--                 z <- runBackT (f y)
--                 case z of
--                  GoBack  -> loop
--                  other -> return other
--            GoBack -> return  GoBack


{-# NOINLINE breturn  #-}

-- | Use this instead of return to return from a computation with an ask statement
--
-- This way when the user press the back button, the computation will execute back, to
-- the returned code, according with the user navigation.
breturn x= BackT . return $ BackPoint x         -- !> "breturn"


instance (MonadIO m) => MonadIO (BackT  m) where
  liftIO f= BackT $ liftIO  f >>= \ x -> return $ NoBack x

instance (Monad m,Functor m) => Functor (BackT m) where
  fmap f g= BackT $ do
     mr <- runBackT g
     case mr of
      BackPoint x  -> return . BackPoint $ f x
      NoBack x     -> return . NoBack $ f x
      GoBack       -> return $ GoBack

{-# NOINLINE liftBackT  #-}
liftBackT f = BackT $ f  >>= \x ->  return $ NoBack x
instance MonadTrans BackT where
  lift f = BackT $ f  >>= \x ->  return $ NoBack x


instance MonadState s m => MonadState s (BackT m) where
   get= lift get                                -- !> "get"
   put= lift . put

type  WState view m = StateT (MFlowState view) m
type FlowM view m=  BackT (WState view m)

data FormElm view a = FormElm [view] (Maybe a) deriving Typeable

newtype View v m a = View { runView :: WState v m (FormElm v a) }


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
  View f <|> View g= View $ 
                   f  >>= \(FormElm form1 k) ->
                   g  >>= \(FormElm form2 x) ->
                   return $ FormElm (form1 ++ form2) (k <|> x)


instance  (Monad m, Functor m) => Monad (View view m) where
  --View view m a-> (a -> View view m b) -> View view m b
    View x >>= f = View $ do
                   FormElm form1 mk <- x
                   case mk of
                     Just k  -> do
                       FormElm form2 mk <- runView $ f k
                       return $ FormElm (form1++ form2) mk
                     Nothing -> return $ FormElm form1 Nothing

    return= View .  return . FormElm  [] . Just 


instance MonadTrans (View view) where
  lift f = View $   lift  f >>= \x ->  return $ FormElm [] $ Just x

instance  (Functor m, Monad m)=> MonadState (MFlowState view) (View view m) where
  get = View $  get >>= \x ->  return $ FormElm [] $ Just x
  put st = View $  put st >>= \x ->  return $ FormElm [] $ Just x


instance (MonadIO m, Functor m) => MonadIO (View view m) where
    liftIO= lift . liftIO

--instance Executable (View v m) where
--  execute f =  execute $  evalStateT  f mFlowState0


--instance (Monad m, Executable m, Monoid view, FormInput view)
--          => Executable (StateT (MFlowState view) m) where
--   execute f= execute $  evalStateT  f mFlowState0

-- | Cached widgets operate with widgets in the Identity monad, but they may perform IO using the execute instance
-- of the monad m, which is usually the IO monad. execute basically \"sanctifies\" the use of unsafePerformIO for a transient purpose
-- such is caching. This is defined in "Data.TCache.Memoization". The user can create his
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

cachedWidget ::(Show a,MonadIO m,Typeable view
        , FormInput view, Typeable a, Functor m, Executable m )
        => String  -- ^ The key of the cached object for the retrieval
        -> Int     -- ^ Timeout of the caching. Zero means sessionwide
        -> View view Identity a   -- ^ The cached widget, in the Identity monad
        -> View view m a          -- ^ The cached result
cachedWidget key t mf = View $ StateT $ \s -> do
        let(FormElm  form _, seq)= execute $ cachedByKey key 0 $ proc mf s{mfCached=True}
        let(FormElm  _ mx2, s2)  = execute $ runStateT  (runView mf)    s{mfSequence= seq,mfCached=True} --  !> ("mfSequence s1="++  show(mfSequence s1)) !> ("mfSequence s="++  show(mfSequence s)) !> ("mfSequence s'="++  show(mfSequence s'))
        let s''=  s{validated = validated s2}
        return (FormElm form mx2, s'')
        where
        proc mf s= runStateT (runView mf) s>>= \(r,_) -> return (r,mfSequence s)


-- | A FormLet instance
class (Functor m, MonadIO m) => FormLet  a  m view where
   digest :: Maybe a
          -> View view m a

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

{- | Execute the @FlowM view m@ monad. It is used as parameter of `hackMessageFlow`
`waiMessageFlow` or `addMessageFlows`

@main= do
   addMessageFlows [(\"noscript\",transient $ runFlow mainf)]
   forkIO . run 80 $ waiMessageFlow
   adminLoop
@
-}
runFlow :: (FormInput view,  Monad m)
        => FlowM view m a -> Token -> m a 
runFlow  f = \ t ->  evalStateT (runBackT $ backp >>  f)  mFlowState0{mfToken=t}  >>= return . fromFailBack  -- >> return ()
  where
  -- to restart the flow in case of going back before the first page of the flow
  backp = breturn()
  fromFailBack (NoBack x)= x
  fromFailBack (BackPoint x)= x

-- | run a persistent flow inside the current flow. It is identified by the procedure and
-- the string identifier.
-- unlike the normal flows, that are infinite loops, runFlowIn executes finite flows
-- once executed, in subsequent executions the flow will return the stored result
-- without asking again. This is useful for asking/storing/retrieving user defined configurations.
runFlowIn
  :: (MonadIO m,
      FormInput view)
  => String
  -> FlowM  view  (Workflow IO)  b
  -> FlowM view m b
runFlowIn wf f= do
  t <-  gets mfToken
  liftIO $ WF.exec1nc wf $ runFlow f t



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
   BackT $ do
    (r,s') <-  lift . WF.step $ runStateT (runBackT f) s
    -- when recovery of a workflow, the MFlow state is not considered
    when( mfSequence s' >0) $ put s'
    return r



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
--     s <- get
--     (r, s') <- lift $ do
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



getParam1 :: (Monad m, MonadState (MFlowState v) m, Typeable a, Read a, FormInput v)
          => String -> Params -> [v] ->  m (FormElm v a)
getParam1 par req form=  r
 where
 r= case lookup  par req of
    Just x -> do
        modify $ \s -> s{validated= True}
        maybeRead x                        -- !> x
    Nothing  -> return $ FormElm form Nothing
 getType ::  m (FormElm v a) -> a
 getType= undefined
 x= getType r
 maybeRead str= do

   if typeOf x == (typeOf  ( undefined :: String))
         then return . FormElm form . Just  $ unsafeCoerce str
         else case readsPrec 0 $ str of
              [(x,"")] ->  return . FormElm form  $ Just x
              _ -> do

                   let err= inred . fromString $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ FormElm  (err:form) Nothing

-- | Validates a form or widget result against a validating procedure
--
-- @getOdd= getInt Nothing `validate` (\x -> return $ if mod x 2==0 then  Nothing else Just "only odd numbers, please")@
validate
  :: (FormInput view, Monad m) =>
     View view m a
     -> (a -> WState view m (Maybe String))
     -> View view m a
validate  formt val= View $ do
   FormElm form mx <- (runView  formt) 
   case mx of
    Just x -> do
      me <- val x
      modify (\s -> s{validated= True})
      case me of
         Just str ->
           --FormElm form mx' <- generateForm [] (Just x) noValidate
           return $ FormElm ( inred (fromString str) : form) Nothing
         Nothing  -> return $ FormElm [] mx
    _ -> return $ FormElm form mx

-- | Actions are callbacks that are executed when a widget is validated.
-- It is useful when the widget is inside widget containers that know nothing about his content.
-- It returns a result  that can be significative or, else, be ignored with '<**' and '**>'.
-- An action may or may not initiate his own dialog with the user via `ask`
waction
  :: (FormInput view, Monad m) =>
     View view m a
     -> (a -> FlowM view m b)
     -> View view m b
waction formt act= View $ do
   FormElm form mx <- (runView  formt) 
   case mx of
    Just x -> do
      modify (\s -> s{validated= True})
      clearEnv
      br <- runBackT $ act x
      case br of
       GoBack -> do
               modify (\s -> s{validated= False})
               return $ FormElm form Nothing
       NoBack r    ->  return . FormElm form $ Just r
       BackPoint r ->  return . FormElm form $ Just r -- bad. no backpoints
        
    _ -> return $ FormElm form Nothing

-- | A modifier get the result and the rendering of a widget and change them.
--
-- This modifier, when logged, changes a login-password-register widget with a display username.
--
-- @userFormOrName= `userWidget` Nothing `userFormLine` \`wmodify\` f
--   where
--   f _ justu\@(Just u)  =  return ([`fromString` u], justu) -- user validated, display and return user
--   f felem Nothing = do
--     us <-  `getCurrentUser`
--     if us == `anonymous`
--           then return (felem, Nothing)                    -- user not logged, present the form
--           else return([`fromString` us],  Just us)        -- already logged, display and return user@
wmodify :: (Monad m, FormInput v)
        => View v m a
        -> ([v] -> Maybe a -> WState v m ([v], Maybe b))
        -> View v m b
wmodify formt act = View $ do
   FormElm f mx <- runView  formt 
   (f',mx') <- act f mx
   return $ FormElm f' mx'



instance (FormInput view, FormLet a m view , FormLet b m view )
          => FormLet (a,b) m view  where
  digest  mxy  = do
      let (x,y)= case mxy of Nothing -> (Nothing, Nothing); Just (x,y)-> (Just x, Just y)
      (,) <$> digest x   <*> digest  y

instance (FormInput view, FormLet a m view , FormLet b m view,FormLet c m view )
          => FormLet (a,b,c) m view  where
  digest  mxy  = do
      let (x,y,z)= case mxy of Nothing -> (Nothing, Nothing, Nothing); Just (x,y,z)-> (Just x, Just y,Just z)
      (,,) <$> digest x  <*> digest  y  <*> digest  z

-- | display a text box and return a String
getString  :: (FormInput view,Monad m) =>
     Maybe String -> View view m String
getString = getTextBox

-- | display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (FormInput view, Functor m, MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

-- | display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (FormInput view, Functor m, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

-- | display a password box 
getPassword :: (FormInput view,
     Monad m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

-- | implement a radio button that perform a submit when pressed.
-- the parameter is the name of the radio group
getRadioActive :: (FormInput view, Functor m, MonadIO m) =>
             String -> String -> View view m  String
getRadioActive  n v= View $ do
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
  FormElm form mn <- getParam1 n env []
  return $ FormElm [finput n "radio" v
          ( isJust mn  && v== fromJust mn) (Just "this.form.submit()")]
          mn

       

-- | implement a radio button
-- the parameter is the name of the radio group
getRadio :: (FormInput view, Functor m, MonadIO m) =>
            String -> String -> View view m  String
getRadio n v= View $ do
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
  FormElm f mn <- getParam1 n env []
  return $ FormElm
      (f++[finput n "radio" v
          ( isJust mn  && v== fromJust mn) Nothing])
      mn

data CheckBoxes = CheckBoxes [String]

instance Monoid CheckBoxes where
  mappend (CheckBoxes xs) (CheckBoxes ys)= CheckBoxes $ xs ++ ys
  mempty= CheckBoxes []

instance (Monad m, Functor m) => Monoid (View v m CheckBoxes) where
  mappend x y=  mappend <$> x <*> y
  mempty= return (CheckBoxes [])

instance (Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- ^ beware that both operands must validate to generate a sum
  mempty= return mempty

-- | display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (FormInput view, Functor m, MonadIO m) =>
                Bool -> String -> View view m  CheckBoxes
setCheckBox checked v= View $ do
  n <- getNewName
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
      strs= map snd $ filter ((==) n . fst) env
      mn= if null strs then Nothing else Just $ head strs
--  FormElm f mn <- getParam1 n env []
  val <- gets validated
  let ret= case val of
        True ->  Just $ CheckBoxes  strs
        False -> Nothing
  return $ FormElm
      ( [ finput n "checkbox" v
        ( checked || (isJust mn  && v== fromJust mn)) Nothing])
      ret

getCheckBoxes ::(FormInput view, Monad m)=> View view m  CheckBoxes -> View view m [String]
getCheckBoxes boxes =  View $ do
    n <- getNewName
    env <- gets mfEnv
    FormElm form (mr :: Maybe String) <- getParam1 n env [finput n "hidden" "" False Nothing]
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    FormElm form2 mr2 <- runView boxes
    return $ FormElm (form ++ form2) $
        case (mr,mr2) of
          (Nothing,_) ->  Nothing
          (Just _,Nothing) -> Just []
          (Just _, Just (CheckBoxes rs))  ->  Just rs




-- get a parameter form the las received response
getEnv ::  MonadState (MFlowState view) m => String -> m(Maybe String)
getEnv n= gets mfEnv >>= return . lookup  n
     
getTextBox
  :: (FormInput view,
      Monad  m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  View view m a
getTextBox ms  = getParam Nothing "text" ms


getParam
  :: (FormInput view,
      Monad m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe String -> String -> Maybe a -> View view m  a
getParam look type1 mvalue = View $ do
    tolook <- case look of
       Nothing  -> getNewName  
       Just n -> return n
    let nvalue= case mvalue of
           Nothing  -> ""
           Just v   ->
             let typev= typeOf v
             in if typev==typeOf (undefined :: String) then unsafeCoerce v
                else if typev==typeOf (undefined :: String) then unsafeCoerce v
                else if typev==typeOf (undefined :: ByteString) then unsafeCoerce v
                else show v
        form= [finput tolook type1 nvalue False Nothing]
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    getParam1 tolook env form
       
    
getNewName :: MonadState (MFlowState view) m =>  m String
getNewName= do
      st <- get
      let n= mfSequence st
      put $ st{mfSequence= n+1}
      let pref= if mfCached st then 'c' else 'p'
      return $  pref : (show n)

getCurrentName :: MonadState (MFlowState view) m =>  m String
getCurrentName= do
     st <- get
     let parm = mfSequence st
     return $ if mfCached st then "c" else "p"++show parm


-- | display a multiline text box and return its content
getMultilineText :: (FormInput view,
      Monad m) =>
      String ->  View view m String
getMultilineText nvalue = View $ do
    tolook <- getNewName
    env <- gets mfEnv
    let form= [ftextarea tolook nvalue]
    getParam1 tolook env form
      
--instance  (MonadIO m, Functor m, FormInput view) => FormLet Bool m view where
--   digest mv =  getBool b "True" "False"
--       where
--       b= case mv of
--           Nothing -> Nothing
--           Just bool -> Just $ case bool of
--                          True ->  "True"
--                          False -> "False"

-- | display a dropdown box with the two values (second (true) and third parameter(false))
-- . With the value of the first parameter selected.                  
getBool :: (FormInput view,
      Monad m) =>
      Bool -> String -> String -> View view m Bool
getBool mv truestr falsestr= View $  do
    tolook <- getNewName
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    r <- getParam1 tolook env $ [fselect tolook(foption1 truestr mv `mappend` foption1 falsestr (not mv))]
    return $ fmap fromstr r
--    case mx of
--       Nothing ->  return $ FormElm f Nothing
--       Just x  ->  return . FormElm f $ fromstr x
    where
    fromstr x= if x== truestr then True else False

-- | display a dropdown box with the options in the first parameter is optionally selected
-- . It returns the selected option. 
getSelect :: (FormInput view,
      Monad m,Typeable a, Read a) =>
      View view m (MFOption a) ->  View view m  a
getSelect opts = View $ do
    tolook <- getNewName
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    FormElm form mr <- (runView opts)
    getParam1 tolook env [fselect tolook $ mconcat form] 

data MFOption a= MFOption

instance (Monad m, Functor m) => Monoid (View view m (MFOption a)) where
  mappend =  (<|>)
  mempty = Control.Applicative.empty

-- | set the option for getSelect. Options are concatenated with `<|>`
setOption n v = setOption1 n v False

-- | set the selected option for getSelect. Options are concatenated with `<|>`
setSelectedOption n v= setOption1 n v True
 
setOption1 :: (FormInput view,
      Monad m, Typeable a, Show a) =>
      a -> view -> Bool ->  View view m  (MFOption a) 
setOption1 nam  val check= View $ do
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    let n= if typeOf nam== typeOf(undefined :: String) then unsafeCoerce nam else show nam
    return . FormElm [foption n val check]  $ Just MFOption


-- | Enclose Widgets in some formating.
-- @view@ is intended to be instantiated to a particular format
--
-- This is a widget, which is table with some links. it returns an Int
--
-- > import MFlow.Forms.XHtml
-- >
-- > tableLinks :: View Html Int
-- > tableLinks= table ! [border 1,thestyle "width:20%;margin-left:auto;margin-right:auto"]
-- >              <<< caption << "choose an item"
-- >              ++> thead << tr << concatHtml[ th << bold << "item", th << bold << "times chosen"]
-- >              ++> (tbody
-- >                   <<< (tr <<< td <<< wlink  0 (bold <<"iphone") <++  td << ( bold << "One")
-- >                   <|>  tr <<< td <<< wlink  1 (bold <<"ipad")   <++  td << ( bold << "Two")
-- >                   <|>  tr <<< td <<< wlink  2 (bold <<"ipod")   <++  td << ( bold << "Three"))
-- >                   )
(<<<) :: (Monad m,  Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
(<<<) v form= View $ do
  FormElm f mx <- runView form 
  return $ FormElm [v $ mconcat f] mx


infixr 5 <<<



-- | Useful for the creation of pages using two or more views.
-- For example 'HSP' and 'Html'.
-- Because both have ConvertTo instances to ByteString, then it is possible
-- to mix them via 'normalize':
--
-- > normalize widget  <+> normalize widget'
--
-- is equivalent to
--
-- > widget .<+>. widget'

normalize ::(Monad m, ToByteString v) => View v m a -> View ByteString m a
normalize f=  View $ StateT $ \s ->do
       (FormElm fs mx, s') <-  runStateT  (runView f) $ unsafeCoerce s
       -- the only diference between the states of the two views is mfHeader
       -- which don't affect to runState 
       return  $ (FormElm (map toByteString fs ) mx,unsafeCoerce s')

class ToByteString a where
  toByteString :: a -> ByteString

instance ToByteString a => ToHttpData a where
  toHttpData = toHttpData . toByteString

instance ToByteString ByteString where
  toByteString= id

instance ToByteString String where
  toByteString  =  pack



-- | Append formatting code to a widget
--
-- @ getString "hi" <++ H1 << "hi there"@
(<++) :: (Monad m)
      => View v m a
      -> v
      -> View v m a 
(<++) form v= View $ do
  FormElm f mx <-  runView  form  
  return $ FormElm ( f ++ [ v]) mx 
 
infixr 6 <++ , .<++. , ++> , .++>.
-- | Prepend formatting code to a widget
--
-- @bold << "enter name" ++> getString Nothing @

(++>) :: (Monad m,  Monoid view)
       => view -> View view m a -> View view m a
html ++> digest =  (html `mappend`) <<< digest




type Name= String
type Type= String
type Value= String
type Checked= Bool
type OnClick= Maybe String


-- | Minimal interface for defining the basic form combinators in a concrete rendering.
-- defined in this module. see "MFlow.Forms.XHtml" for the instance for @Text.XHtml@ and MFlow.Forms.HSP for an instance
-- form Haskell Server Pages.
class Monoid view => FormInput view where
    ftag :: String -> view 
    inred   :: view -> view
    fromString :: String -> view
    flink ::  String -> view -> view 
    flink1:: String -> view
    flink1 verb = flink verb (fromString  verb) 
    finput :: Name -> Type -> Value -> Checked -> OnClick -> view 
    ftextarea :: String -> String -> view
    fselect :: String ->  view -> view
    foption :: String -> view -> Bool -> view
    foption1 :: String -> Bool -> view
    foption1   val msel= foption val (fromString val) msel
    formAction  :: String -> view -> view
    addAttributes :: view -> Attribs -> view

-- | add attributes to the form element
-- if the view has more than one element, it is applied to  the first
infix 8 <!
widget <! atrs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  [addAttributes (head fs) atrs] mx


-------------------------------

--
--
--instance (MonadIO m, Functor m, FormInput view)
--         => FormLet User m view where
--       digest muser=
--        (User <$>  getString ( userName <$> muser)
--              <*>  getPassword)
--        `validate` userValidate


newtype Lang= Lang String

data MFlowState view= MFlowState{   
   mfSequence :: Int,
   mfCached   :: Bool,
   prevSeq    :: [Int],
   onInit     :: Bool,
   validated  :: Bool,
--   mfUser     :: String,
   mfLang     :: Lang,
   mfEnv      :: Params,
   needForm   :: Bool,
   hasForm    :: Bool,
--   mfServer   :: String,
--   mfPath     :: String,
--   mfPort     :: Int,

   mfToken     :: Token,
   mfkillTime :: Int,
   mfSessionTime :: Integer,
   mfCookies   :: [Cookie],
   mfHeader ::  view -> view,
   mfDebug  :: Bool
   }
   deriving Typeable

stdHeader v = v


mFlowState0 :: (FormInput view) => MFlowState view
mFlowState0= MFlowState 0 False [] True  False  (Lang "en") [] False False (error "token of mFlowState0 used") 0 0 [] stdHeader False

-- | Set the header-footer that will enclose the widgets. It must be provided in the
-- same formatting than them, altrough with normalization to byteStrings can be used any formatting
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

setHeader :: Monad m => (view -> view) -> FlowM view m ()
setHeader header= do
  fs <- get
  put fs{mfHeader= header}



-- | return the current header
getHeader :: Monad m => FlowM view m (view -> view)
getHeader= gets mfHeader

-- | Set an HTTP cookie
setCookie :: MonadState (MFlowState view) m
          => String  -- ^ name
          -> String  -- ^ value
          -> String  -- ^ path
          -> Maybe Integer   -- ^ Max-Age in seconds. Nothing for a session cookie
          -> m ()
setCookie n v p me= do
    modify $ \st -> st{mfCookies=  (n,v,p,fmap  show me):mfCookies st }

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

type UserStr= String
type PasswdStr= String
-- | Is an example of login\/register validation form needed by 'userWidget'. In this case
-- the form field appears in a single line. it shows, in sequence, entries for the username,
-- password, a button for loging, a entry to repeat password necesary for registering
-- and a button for registering.
-- The user can build its own user login\/validation forms by modifying this example
--
-- @ userFormLine=
--     (User \<\$\> getString (Just \"enter user\") \<\*\> getPassword \<\+\> submitButton \"login\")
--     \<\+\> fromString \"  password again\" \+\> getPassword \<\* submitButton \"register\"
-- @
userFormLine :: (FormInput view, Functor m, Monad m)
            => View view m (Maybe (UserStr,PasswdStr), Maybe PasswdStr)
userFormLine=
       ((,)  <$> getString (Just "enter user")                  <! [("size","5")]
             <*> getPassword                                    <! [("size","5")]
         <+> submitButton "login")
         <** (fromString "  password again" ++> getPassword      <! [("size","5")]
         <*  submitButton "register")

-- | Example of user\/password form (no validation) to be used with 'userWidget'
userLogin :: (FormInput view, Functor m, Monad m)
            => View view m (Maybe (UserStr,PasswdStr), Maybe String)
userLogin=
        ((,)  <$> fromString "Enter User: " ++> getString Nothing     <! [("size","4")]
              <*> fromString "  Enter Pass: " ++> getPassword                       <! [("size","4")]
              <** submitButton "login")
              <+> (noWidget
              <*  noWidget)



-- | empty widget that return Nothing. May be used as \"empty boxes\" inside larger widgets
noWidget ::  (FormInput view,
     Monad m) =>
     View view m a
noWidget= View . return $ FormElm  [] Nothing

-- | render the Show instance of the parameter and return it. It is useful
-- for displaying information
wrender :: (Monad m, Show a, FormInput view) => a -> View view m a
wrender x= View . return $ FormElm [fromString $ show x] (Just x)

-- | Wether the user is logged or is anonymous
isLogged :: MonadState (MFlowState v) m => m Bool
isLogged= do
   rus <-  return . tuser =<< gets mfToken
   return . not $ rus ==  anonymous

-- | It creates a widget for user login\/registering. If a user name is specified
-- in the first parameter, it is forced to login\/password as this specific user.
-- Otherwise, if the user is already logged, the widget does not appear
-- If the user press the register button, the user/password is registered and the
-- user logged.
userWidget :: ( MonadIO m, Functor m
          , FormInput view) 
         => Maybe String
         -> View view m (Maybe (UserStr,PasswdStr), Maybe String)
         -> View view m String
userWidget muser formuser= wform formuser `validate` val muser `waction` login
   where
   val _ (Nothing,_) = return $ Just "Plese fill in the user/passwd to login, or user/passwd/passwd to register"

   val mu (Just us, Nothing)=
        if isNothing mu || isJust mu && fromJust mu == fst us
           then userValidate us
           else return $ Just "wrong user for the operation"

   val mu (Just us, Just p)=
      if isNothing mu || isJust mu && fromJust mu == fst us
        then  if  length p > 0 && snd us== p
                  then return Nothing
                  else return $ Just "The passwords do not match"
        else return $ Just "wrong user for the operation"

   val _ _ = return $ Just "Please fill in the fields for login or register"

   login  (Just (u,p), Nothing)= do
         let uname= u
         st <- get
         let t = mfToken st
             t'= t{tuser= uname}
         moveState (twfname t) t t'
         put st{mfToken= t'}
         liftIO $ deleteTokenInList t
         liftIO $ addTokenToList t'
         setCookie cookieuser   uname "/" Nothing   -- !> "setcookie"
         return uname

   login (Just us@(u,p), Just _)=  do
         userRegister u p
         login (Just us , Nothing)


-- | If not logged, perform login. otherwise return the user
--
-- @getUserSimple= getUser Nothing userFormLine@
getUserSimple :: ( FormInput view, Typeable view
                 , ToHttpData view
                 , MonadIO m, Functor m)
              => FlowM view m String
getUserSimple= getUser Nothing userFormLine

-- | Very basic user authentication. The user is stored in a cookie.
-- it looks for the cookie. If no cookie, it ask to the user for a `userRegister`ed
-- user-password combination.
-- The user-password combination is only asked if the user has not logged already
-- otherwise, the stored username is returned.
--
-- @getUser mu form= ask $ userWidget mu form@
getUser :: ( FormInput view, Typeable view
           , ToHttpData view
           , MonadIO m, Functor m)
          => Maybe String
          -> View view m (Maybe (UserStr,PasswdStr), Maybe String)
          -> FlowM view m String
getUser mu form= ask $ userWidget mu form


--instance   (MonadIO m, Functor m, m1 ~ m, b ~ a)
--           => Widget(View view m1 b) a m view where
--    widget  =  id 

-- | Join two widgets in the same page
-- the resulting widget, when `ask`ed with it, return a 2 tuple of their validation results
--
--  > r <- ask  widget1 <+>  widget2
--  > case r of (Just x, Nothing) -> ..
(<+>) , mix ::  Monad m
      => View view m a
      -> View view m b
      -> View view m (Maybe a, Maybe b)
mix digest1 digest2= View $ do
  FormElm f1 mx' <- runView  digest1
  FormElm f2 my' <- runView  digest2
  return $ FormElm (f1++f2) 
         $ case (mx',my') of
              (Nothing, Nothing) -> Nothing
              other              -> Just other

infixr 2 <+>, .<+>.

(<+>)  = mix


infixr 3  **> , .**>. ,  <** , .<**.
-- | The first elem result (even if it is not validated) is discarded, and the secod is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the first elem fails.
--
-- The first element is displayed however, as happens in the case of '*>' .
--
-- Here @w\'s@ are widgets and @r\'s@ are returned values
--
--   @(w1 <* w2)@  will return @Just r1@ only if w1 and w2 are validated
--
--   @(w1 <** w2)@ will return @Just r1@ even if w2 is not validated
--


(**>) :: (Functor m, Monad m)
      => View view m a -> View view m b -> View view m b
(**>) form1 form2 = valid form1 *> form2


-- | The second elem result (even if it is not validated) is discarded, and the first is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the second elem fails.
-- The second element is displayed however, as in the case of '<*'.
-- see the `<**` examples
(<**)
  :: (Functor m, Monad m) =>
     View view m a -> View view m b -> View view m a
(<**) form1 form2 =  form1 <* valid form2


valid form= View $ do
   FormElm form mx <- runView form
   return $ FormElm form $ Just undefined


-- | It is the way to interact with the user.
-- It takes a widget and return the user result.
-- If the environment has the result, ask don't ask to the user.
-- To force asking in any case, put an `clearEnv` statement before
ask
  :: (
      ToHttpData view,
      FormInput view,
      MonadIO m,
      Typeable view) =>
      View view m b -> FlowM view m b
ask x =   do
     st1 <- get
     let st= st1{hasForm= False, needForm= False,validated= False} 
     put st
     FormElm forms mx <- lift $ runView  x  -- !> "runWidget"
     st' <-  get
     case mx    of 
       Just x -> do
         put st'{prevSeq= mfSequence st: prevSeq st',onInit= True ,mfEnv=[]}
         breturn x                                 -- !> "just x"
             
       _ ->
         if  not (validated st') && not (onInit st') && hasParams (mfSequence st') ( mfEnv st')  -- !> (show $ validated st')  !> (show $ onInit st')
          then do
             put st'{mfSequence= head1 $ prevSeq st'
                    ,prevSeq= tail1 $ prevSeq st' }
             fail ""                           -- !> "repeatPlease"
          else do
             let header= mfHeader st'
                 t= mfToken st'
                 cont = case (needForm st', hasForm st') of
                   (True, False) ->  header $ formAction (twfname t) $ mconcat forms
                   _             ->  header $ mconcat  forms

             let HttpData ctype c s= toHttpData cont 
             liftIO . sendFlush t $ HttpData ctype (mfCookies st' ++ c) s
             put st{mfCookies=[], onInit= False, mfToken= t }                --    !> ("after "++show ( mfSequence st'))
             receiveWithTimeouts
             ask x
    where
    head1 []=0
    head1 xs= head xs
    tail1 []=[]
    tail1 xs= tail xs

    hasParams seq= not . null . filter (\(p,_) ->

       let tailp = tail p in
       (head p== 'p' || head p == 'c')
       && and (map isNumber tailp)
       && read  tailp <= seq)

-- | True if the flow is going back (as a result of the back button pressed in the web browser).
--  Usually this chech is nos necessary unless conditional code make it necessary
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
-- However this is very specialized. normally the back button detection is not necessary.
-- In a persistent flow (with step) even this default entry option would be completely automatic,
-- since the process would restar at the last page visited. No setting is necessary.
goingBack :: MonadState (MFlowState view) m => m Bool
goingBack = do
    st <- get
    return $ not (validated st) && not (onInit st)

-- | Clears the environment
clearEnv :: MonadState (MFlowState view) m =>  m ()
clearEnv= do
  st <- get
  put st{ mfEnv= []}

receiveWithTimeouts :: MonadIO m => FlowM view m ()
receiveWithTimeouts= do
         st <- get
         let t= mfToken st
             t1= mfkillTime st
             t2= mfSessionTime st
         req <- return . getParams =<< liftIO ( receiveReqTimeout t1 t2  t)
         put st{mfEnv= req}
        



-- | wrap a widget of form element within a form-action element.
---- Usually it is done automatically by the @Wiew@ monad.
wform ::  (Monad m, FormInput view)
          => View view m b -> View view m b

wform x = View $ do
         FormElm form mr <- (runView $   x )
         st <- get
         let t = mfToken  st
         anchor <- getNewName
         put st{hasForm= True}
         let form1= formAction (twfname t++"#"++anchor) $  mconcat form  !> anchor

         return $ FormElm [(ftag "a") `addAttributes` [("name",anchor)], form1] mr

resetButton :: (FormInput view, Monad m) => String -> View view m () 
resetButton label= View $ return $ FormElm [finput  "reset" "reset" label False Nothing]   $ Just ()

submitButton :: (FormInput view, Monad m) => String -> View view m String
submitButton label= getParam Nothing "submit" $ Just label


--insertView view= View $ return $ FormElm [view] $ Just ()
--
--infix 3 +>
--view +> widget= (insertView view) *> widget
--
--infix 3 <+
--widget <+ view =  widget <* insertView view



-- | creates a link wiget. A link can be composed with other widget elements,
wlink :: (Typeable a, Read a, Show a, MonadIO m, Functor m, FormInput view) 
         => a -> view -> View  view m a
wlink x v= View $ do
      verb <- getWFName
      name <- getNewName
      env  <- gets mfEnv 
      let
          showx= if typeOf x== typeOf (undefined :: String) then unsafeCoerce x else show x
          toSend = flink (verb ++ "?" ++  name ++ "=" ++ showx) v
      getParam1 name env [toSend]




--instance (Widget a b m view, Monoid view) => Widget [a] b m view where
--  widget xs = View $ do
--      forms <- mapM(\x -> (runView  $  widget x )) xs
--      let vs  = concatMap (\(FormElm v _) -> v) forms
--          res = filter isJust $ map (\(FormElm _ r) -> r) forms
--          res1= if null res then Nothing else head res
--      return $ FormElm [mconcat vs] res1

-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (Monoid view, MonadIO m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= View $ do 
      forms <- mapM(\x -> (runView   x )) xs
      let vs  = concatMap (\(FormElm v _) ->  [mconcat v]) forms
          res = filter isJust $ map (\(FormElm _ r) -> r) forms
          res1= if null res then Nothing else head res
      return $ FormElm  vs res1



-- | intersperse a widget in a list of widgets. the results is a 2-tuple of both types
(|*>) :: (MonadIO m, Functor m,Monoid view)
            => View view m r
            -> [View view m r']
            -> View view m (Maybe r,Maybe r')
(|*>) x xs= View $ do
  FormElm fxs rxs <-  runView $ firstOf  xs
  FormElm fx rx   <- runView $  x

  return $ FormElm (fx ++ intersperse (mconcat fx) fxs ++ fx)
         $ case (rx,rxs) of
            (Nothing, Nothing) -> Nothing
            other -> Just other



infixr 5 |*>, .|*>.

-- | Put a widget above and below other. Useful for navigation links in a page.
(|+|) :: (Functor m, Monoid view, MonadIO m)
      => View view m r
      -> View view m r'
      -> View view m (Maybe r, Maybe r')
(|+|) w w'=  w |*> [w']

infixr 1 |+|, .|+|.


-- | Flatten a binary tree of tuples of Maybe results produced by the \<+> operator
-- into a single tuple with the same elements in the same order.
-- This is useful for easing matching. For example:
--
-- @ res \<- ask $ wlink1 \<+> wlink2 wform \<+> wlink3 \<+> wlink4@
--
-- @res@  has type:
--
-- @Maybe (Maybe (Maybe (Maybe (Maybe a,Maybe b),Maybe c),Maybe d),Maybe e)@
--
-- but @flatten res@ has type:
--
-- @ (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e)@

flatten :: Flatten (Maybe tree) list => tree -> list
flatten res= doflat $ Just res

class Flatten tree list  where
 doflat :: tree -> list


type Tuple2 a b= Maybe (Maybe a, Maybe b)
type Tuple3 a b c= Maybe ( (Tuple2 a b), Maybe c)
type Tuple4 a b c d= Maybe ( (Tuple3 a b c), Maybe d)
type Tuple5 a b c d e= Maybe ( (Tuple4 a b c d), Maybe e)
type Tuple6 a b c d e f= Maybe ( (Tuple5 a b c d e), Maybe f)

instance Flatten (Tuple2 a b) (Maybe a, Maybe b) where
  doflat (Just(ma,mb))= (ma,mb)
  doflat Nothing= (Nothing,Nothing)

instance Flatten (Tuple3 a b c) (Maybe a, Maybe b,Maybe c) where
  doflat (Just(mx,mc))= let(ma,mb)= doflat mx in (ma,mb,mc)
  doflat Nothing= (Nothing,Nothing,Nothing)

instance Flatten (Tuple4 a b c d) (Maybe a, Maybe b,Maybe c,Maybe d) where
  doflat (Just(mx,mc))= let(ma,mb,md)= doflat mx in (ma,mb,md,mc)
  doflat Nothing= (Nothing,Nothing,Nothing,Nothing)

instance Flatten (Tuple5 a b c d e) (Maybe a, Maybe b,Maybe c,Maybe d,Maybe e) where
  doflat (Just(mx,mc))= let(ma,mb,md,me)= doflat mx in (ma,mb,md,me,mc)
  doflat Nothing= (Nothing,Nothing,Nothing,Nothing,Nothing)

instance Flatten (Tuple6 a b c d e f) (Maybe a, Maybe b,Maybe c,Maybe d,Maybe e,Maybe f) where
  doflat (Just(mx,mc))= let(ma,mb,md,me,mf)= doflat mx in (ma,mb,md,me,mf,mc)
  doflat Nothing= (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

infixr 7 .<<.
-- | > (.<<.) w x = w $ toByteString x
(.<<.) :: (ToByteString view) => (ByteString -> ByteString) -> view -> ByteString
(.<<.) w x = w ( toByteString x)

-- | > (.<+>.) x y = normalize x <+> normalize y
(.<+>.)
  :: (Monad m, ToByteString v, ToByteString v1) =>
     View v m a -> View v1 m b -> View ByteString m (Maybe a, Maybe b)
(.<+>.) x y = normalize x <+> normalize y

-- | > (.|*>.) x y = normalize x |*> map normalize y
(.|*>.)
  :: (Functor m, MonadIO m, ToByteString v, ToByteString v1) =>
     View v m r
     -> [View v1 m r'] -> View ByteString m (Maybe r, Maybe r')
(.|*>.) x y = normalize x |*> map normalize y

-- | > (.|+|.) x y = normalize x |+| normalize y
(.|+|.)
  :: (Functor m, MonadIO m, ToByteString v, ToByteString v1) =>
     View v m r -> View v1 m r' -> View ByteString m (Maybe r, Maybe r')
(.|+|.) x y = normalize x |+| normalize y

-- | > (.**>.) x y = normalize x **> normalize y
(.**>.)
  :: (Monad m, Functor m, ToByteString v, ToByteString v1) =>
     View v m a -> View v1 m b -> View ByteString m b
(.**>.) x y = normalize x **> normalize y

-- | > (.<**.) x y = normalize x <** normalize y
(.<**.)
  :: (Monad m, Functor m, ToByteString v, ToByteString v1) =>
     View v m a -> View v1 m b -> View ByteString m a
(.<**.) x y = normalize x <** normalize y

-- | > (.<|>.) x y= normalize x <|> normalize y
(.<|>.)
  :: (Monad m, Functor m, ToByteString v, ToByteString v1) =>
     View v m a -> View v1 m a -> View ByteString m a
(.<|>.) x y= normalize x <|> normalize y

-- | > (.<++.) x v= normalize x <++ toByteString v
(.<++.) :: (Monad m, ToByteString v, ToByteString v') => View v m a -> v' -> View ByteString m a
(.<++.) x v= normalize x <++ toByteString v

-- | > (.++>.) v x= toByteString v ++> normalize x
(.++>.) :: (Monad m, ToByteString v, ToByteString v') => v -> View v' m a -> View ByteString m a
(.++>.) v x= toByteString v ++> normalize x


instance FormInput  ByteString  where
    ftag x= btag x [] mempty
    inred = btag "b" [("style", "color:red")]
    finput n t v f c= btag "input"  ([("type", t) ,("name", n),("value",  v)] ++ if f then [("checked","true")]  else []
                              ++ case c of Just s ->[( "onclick", s)]; _ -> [] ) ""
    ftextarea name text= btag "textarea"  [("name", name)]   $ pack text

    fselect name   options=  btag "select" [("name", name)]   options

    foption value content msel= btag "option" ([("value",  value)] ++ selected msel)   content
            where
            selected msel = if  msel then [("selected","true")] else []

    addAttributes = addAttrs


    formAction action form = btag "form" [("action", action),("method", "post")]  form
    fromString = pack


    flink  v str = btag "a" [("href",  v)]  str



