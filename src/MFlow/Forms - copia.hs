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
#-}

{- | This module defines an integrated way to interact with the user. `ask` is
the single method of user interaction. it send user interfaces and return statically
typed responses. The user interface definitions are  based on  formLets.
The interaction with the user is  stateful. There may be  many
request-response interactions in the same computation. Therefore the entire user
navigation flow can be coded in a single procedure, just like in a console program.  


But additionally, unlike formLets in its current form, it permits the
 definition of widgets. A widget is data that, when renderized
and interact with the user, return data, just like a formlet, but it hasn't
to be an HTML form. it can contain JavaScript, or additional Html decoration or
it can use Ajax istead of form post for the interaction.
There is an example of widget defined (`Selection`) widgets (and formlets) can be combined in a sigle Html page.


`step` permits persistent server procedures that remember the session state even after
system shutdowns. This is transparent. There is no explicit management of state
the progammer set the process timeout and the session timeout with `setTimeouts`
wether the procedure has been stopped due to the process timeout or due to a system shutdowm,
the procedure restart in the last state when a request for this procedure arrives.
`transient` procedures have no persistent session state and `stateless` procedures accept
a single request and return the response.

Here is a ready-to-run example that combines a Widget (Selection) and
a HTML decorated formLet in the same page.the data entered by the user is
stored in the persistent session state. when the program is restarted, the
entries are recovered.

@
import "MFlow.Hack.XHtml.All"

import Data.Typeable
import Control.Monad.Trans
import qualified Data.Vector as V

main= do

   putStrLn $ options messageFlows
   'run' 80 $ 'hackMessageFlow' messageFlows
   where
   messageFlows=  [(\"main\",  runFlow mainProds )
                  ,(\"hello\", stateless hello)]
   options msgs= \"in the browser choose\\n\\n\" ++
     concat [ "http:\/\/server\/"++ i ++ "\n" | (i,_) \<- msgs]


\--an stateless procedure, as an example
hello :: 'Env' -> IO String
hello env =  return  \"hello, this is a stateless response\"


data Prod= Prod{pname :: String, pprice :: Int} deriving (Typeable,Read,Show)

\-- formLets can have Html formatting. Additional operators \<\++ \<+\> \<\<\< ++\> to XHtml formatting

instance 'FormLet' Prod IO Html where
   'digest' mp= table \<\<\< (
      Prod \<\$\> tr \<\<\< (td \<\< \"enter the name\"  \<++ td \<\<\< getString (pname \<\$\> mp))
           \<\*\> tr \<\<\< (td \<\< \"enter the price\" \<++ td \<\<\< getInt ( pprice \<\$\> mp)))


\-- Here an example of predefined widget (`Selection`) that return an Int, combined in the same
\-- page with the fromLet for the introduction of a product.
\-- The result of the user interaction is Either one or the other value

shopProds :: V.Vector Int -\> [Prod]
          -\> 'View' Html IO  (Either Int Prod)
shopProds cart products=

  p \<\< \"\--\--\--\--\--\--\--\--Shopping List\--\--\--\--\--\--\--\"
  \<++
  widget(Selection{
       stitle = bold \<\< \"choose an item\",
       sheader= [ bold \<\< \"item\"   , bold \<\< \"price\", bold \<\< \"times chosen\"],
       sbody= [([toHtml pname, toHtml \$ show pprice, toHtml \$ show \$ cart V.! i],i )
              | (Prod{..},i ) \<- zip products [1..]]})

  \<+\>
  p \<\< \"\--\--\--\--\--\--\--Add a new product \--\--\--\--\--\--\---\"
  \<++
  table \<\<\< (tr \<\<\< td ! [valign \"top\"]
                          \<\<\< widget (Form (Nothing :: Maybe Prod) )
             ++\>
             tr \<\< td ! [align \"center\"]
                          \<\< hotlink  \"hello\"
                                      (bold \<\< \"Hello World\"))

\-- the header

appheader user forms= thehtml
         \<\< body \<\< dlist \<\< (concatHtml
            [dterm \<\<(\"Hi \"++ user)
            ,dterm \<\< \"This example contains two forms enclosed within user defined HTML formatting\"
            ,dterm \<\< \"The first one is defined as a Widget, the second is a formlet formatted within a table\"
            ,dterm \<\< \"both are defined using an extension of the FormLets concept\"
            ,dterm \<\< \"the form results are statically typed\"
            ,dterm \<\< \"The state is implicitly logged. No explicit handling of state\"
            ,dterm \<\< \"The program logic is written as a procedure. Not    in request-response form. But request response is possible\"
            ,dterm \<\< \"lifespan of the serving process and the execution state defined by the programmer\"
            ,dterm \<\< \"user state is  automatically recovered after cold re-start\"
            ,dterm \<\< \"transient, non persistent states possible.\"
            ])
            +++ forms

\-- Here the procedure. It ask for either entering a new product
\-- or to \"buy\" one of the entered products.
\-- There is a timeout of ten minutes before the process is stopped
\-- THERE IS A timeout of one day for the whole state so after this, the
\-- user will see the list erased.
\-- The state is user specific.

\--mainProds ::  FlowM Html (Workflow IO) ()
mainProds   = do
   setTimeouts (10\*60) (24\*60\*60)
   setHeader \$ \w -\> bold \<\< \"Please enter user/password (pepe/pepe)\" +++ br +++ w


   setHeader  \$ appheader "user"
   mainProds1 [] \$ V.fromList [0]
   where
   mainProds1  prods cart=  do
     mr \<- step . ask  \$ shopProds  cart prods
     case mr of
      Right prod -\> mainProds1  (prod:prods) (V.snoc cart 0)
      Left i   -\> do
         let newCart= cart V.// [(i, cart V.! i + 1 )]
         mainProds1 prods newCart
@

-}

module MFlow.Forms(

{- basic definitions -}
Widget(..),FormLet(..)
,View(..), FormInput(..),FormElm(..), wlink, wform,
getWFName,getNewName
{- widget instances -}
,Selection(..)

{- users -}
,userRegister, userValidate, User(..)
,getCurrentUser, getUser, formUserLine, userWidget,
-- * user interaction
ask, clearEnv,
-- * getters to be used in instances of `FormLet` and `Widget` in the Applicative style.

getString,getInt,getInteger
,getMultilineText,getBool,getOption, getPassword,
getRadio, getRadioActive,
submitButton,resetButton,
validate,
-- * formatting and combining widgets
(<<<),(<++),(++>),(<+>), (<!),(|*>),(|+|), (**>),(<**), flatten
-- * running the flow monad
,FlowM,runFlow,MFlow.Forms.step, MFlow.Forms.stepDebug
-- * setting parameters
,setHeader
,setTimeouts
-- * Cookies
,setCookie
)
where
import Data.TCache
--import Data.Persistent.Queue
import MFlow
import MFlow.Cookies
import Data.RefSerialize hiding((<|>))
import Data.ByteString.Lazy.Char8(ByteString,pack,unpack)
import Control.Workflow as WF
import Data.Typeable
import Data.Monoid
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Workflow(exec1,Workflow, waitUntilSTM, step, unsafeIOtoWF)
import Unsafe.Coerce
import Data.List(intersperse)


import System.IO.Unsafe

import Debug.Trace
(!>)= flip trace


type UserName=  String

data User= User
            { userName :: UserName
            , upassword :: String
            } deriving (Read, Show, Typeable)

eUser= User (error1 "username") (error1 "password")


error1 s= error $ s ++ " undefined"

userPrefix= "User#"
instance Indexable User where
   key User{userName=   user}= keyUserName user

keyUserName n= userPrefix++n





maybeError  err iox = runMaybeT iox >>= \x ->
  case x of
    Nothing -> error err
    Just x -> return x

-- | register an user/password 
userRegister :: MonadIO m => String -> String  -> m()
userRegister user password  = userRegister' $ User user password

userRegister' :: MonadIO m => User  -> m()
userRegister' us= liftIO $
        withResources [us] $ \mus -> do
                       case mus of
                         [Nothing] -> [us]
                         [Just us] -> []



-- | authentication against `userRegister`ed users.
-- to be used with `validate`
userValidate :: MonadIO m =>  User -> m (Maybe String)
userValidate user@User{..} = liftIO $ atomically
     $ withSTMResources [user]
     $ \ mu -> case mu of
         [Nothing] -> resources{toReturn= err }
         [Just (User _ p )] -> resources{toReturn=
               case upassword==p  of
                 True -> Nothing
                 False -> err
               }

     where
     err= Just "Username or password invalid"

--test= runBackT $ do
--        liftRepeat $ print "hola"
--        n2 <- lift $ getLine
--        lift $ print "n3"
--
--        n3  <- lift $  getLine
--        if n3 == "back"
--                   then  fail ""
--                  else lift $ print  $ n2++n3


data FailBack a = BackPoint a | NoBack a | GoBack  deriving (Show,Typeable)

fromFailBack :: Monad m => m (FailBack b) -> m b
fromFailBack f= f >>= \mr -> case mr of
   BackPoint x -> return x
   NoBack x -> return x
   GoBack -> error "fromFailBack: GoBack"

instance (Serialize a) => Serialize (FailBack a ) where
   showp (BackPoint x)= insertString (pack iCanFailBack) >> showp x
   showp (NoBack x)= insertString (pack noFailBack) >> showp x
   showp GoBack  = insertString (pack repeatPlease)

   readp = choice [icanFailBackp,repeatPleasep,noFailBackp]
    where
    noFailBackp   = {-# SCC "deserialNoBack" #-} symbol noFailBack >> readp >>= return . NoBack
    icanFailBackp = {-# SCC "deserialBackPoint" #-} symbol iCanFailBack >> readp >>= return . BackPoint
    repeatPleasep = {-# SCC "deserialbackPlease" #-} symbol repeatPlease >> return  GoBack

iCanFailBack= "B"
repeatPlease= "G"
noFailBack= "N"

newtype BackT m a = BackT { runBackT :: m (FailBack a ) }

instance Monad m => Monad (BackT  m) where
    fail   _ = BackT $ return GoBack
    return x = BackT . return $ NoBack x
    x >>= f  = BackT $ loop
     where
     loop = do
        v <- runBackT x
        case v of
            NoBack y  -> runBackT (f y)
            BackPoint y  -> do
                 z <- runBackT (f y)
                 case z of
                  GoBack  -> loop
                  other -> return other
            GoBack -> return  GoBack


backPointReturn x= BackT . return $ BackPoint x
liftBackPoint f= BackT $ f >>= \x -> return $ BackPoint x
backPointHere :: Monad m => BackT m ()
backPointHere = backPointReturn ()

instance (MonadIO m) => MonadIO (BackT  m) where
  liftIO f= BackT $ liftIO  f >>= \ x -> return $ NoBack x

instance (Monad m,Functor m) => Functor (BackT m) where
  fmap f g= BackT $ do
     mr <- runBackT g
     case mr of
      BackPoint x  -> return . BackPoint $ f x
      NoBack x     -> return . NoBack $ f x
      GoBack       -> return   GoBack

instance MonadTrans BackT where
  lift f= BackT $ f >>= \x ->  return $ NoBack x

instance MonadState s m => MonadState s (BackT m) where
   get= lift get
   put= lift . put

type  WState view m = StateT (MFlowState view) m
type FlowM view m=  BackT (WState view m)
data FormElm view a = FormElm [view] (Maybe a)
newtype View v m a = View { runView :: WState v m (FormElm v a) }

instance (FormInput v, Monoid v)
   => Serialize (MFlowState v) where
  showp s= case mfDebug s of
      False -> showp(mfSequence s)
      True  -> showp(mfSequence s,mfEnv s)
  readp= choice[nodebug, debug]
   where
   nodebug= readp  >>= \n -> return  mFlowState0{mfSequence= n}
   debug=  do
    (n,env) <- readp
    return  (mFlowState0{mfEnv= env, mfSequence= n})



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


-- | a FormLet instance
class (Functor m, MonadIO m) => FormLet  a  m view where
   digest :: Maybe a
          -> View view m a



-- | Minimal definition: either (wrender and wget) or widget
class (Functor m, MonadIO m) => Widget  a b m view |  a -> view where
--   wrender :: a ->(FlowM view m) [view]
--   wrender x =  runView (widget x) >>= \(FormElm frm _) -> return frm
--   wget :: a -> (FlowM view m) (Maybe b)
--   wget x=  runView (widget x) >>= \(FormElm _ mx) -> return mx

   widget :: a  -> View view m b
--   widget x = View $  do
--       form <- wrender x 
--       got  <- wget x 
--       return $ FormElm form got


instance FormLet  a m view => Widget (Maybe a) a m view  where
   widget = digest


runFlow :: (FormInput view, Monoid view, Monad m)
        => FlowM view m () -> Token -> m ()
runFlow  f = \ t ->  evalStateT (runBackT $ backp >> f)  mFlowState0{mfToken=t}  >> return ()
  where
  -- to restart the flow in case of going back before the first page of the flow
  backp = liftBackPoint $ modify $ \st -> st{mfGoingBack=False}


step
  :: (Serialize a,
      Typeable view,
      FormInput view,
      Monoid view,
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

stepDebug
  :: (Serialize a,
      Typeable view,
      FormInput view,
      Monoid view,
      MonadIO m,
      Typeable a) =>
      FlowM view m a
      -> FlowM view (Workflow m) a
stepDebug f= BackT  $ do
     s <- get
     (r, s') <- lift $ do
              (r',stat)<- do
                     rec <- isInRecover
                     case rec of
                          True ->do (r',  s'') <- getStep 0
                                    return (r',s{mfEnv= mfEnv (s'' `asTypeOf`s)})
                          False -> return (undefined,s)
              (r'', s''') <- WF.stepDebug  $ runStateT  (runBackT f) stat >>= \(r,s)-> return (r, s)
              return $ (r'' `asTypeOf` r', s''' )
     put s'
     return r



getParam1 :: ( Typeable a, Read a) => String -> Params ->  Maybe a
getParam1 str req=  r
 where
 r= case lookup str req of
    Just x -> maybeRead x
    Nothing  -> Nothing

 maybeRead str=
   if typeOf r == (typeOf $ Just "")
         then Just  $ unsafeCoerce str
         else case readsPrec 0 str of
              [(x,"")] ->  Just x
              _ -> Nothing

-- | Validates a form or widget result against a validating procedure
--
-- getOdd= getInt Nothing `validate` (\x-> return $ if mod x 2==0 then  Nothing else Just "only odd number please")
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
      case me of
         Just str ->
           --FormElm form mx' <- generateForm [] (Just x) noValidate
           return $ FormElm ( inred (fromString str) : form) Nothing
         Nothing  -> return $ FormElm [] mx
    _ -> return $ FormElm form mx

--generateForm :: 
----  :: (FormInput view ) =>
--       a ->  FlowM view m (FormElm view b)
--generateForm  mx  = do
--
--     (runView   mx ) 




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

getString  :: (FormInput view,Monad m) =>
     Maybe String -> View view m String
getString = getElem

getInteger :: (FormInput view, Functor m, MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getElem

getInt :: (FormInput view, Functor m, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getElem

getPassword :: (FormInput view,
     Monad m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

getRadioActive :: (FormInput view, Functor m, MonadIO m) =>
             String -> String -> View view m  String
getRadioActive  n v= View $ do
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
  let mn = getParam1 n env
  return $ FormElm
       [finput n "radio" v
          ( isJust mn  && v== fromJust mn) (Just "this.form.submit()")]
       mn   -- !> (show mn)



getRadio :: (FormInput view, Functor m, MonadIO m) =>
            String -> String -> View view m  String
getRadio n v= View $ do
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
  let mn = getParam1 n env
  return $ FormElm
      [finput n "radio" v
          ( isJust mn  && v== fromJust mn) Nothing]
      mn
     
getElem
  :: (FormInput view,
      Monad  m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  View view m a
getElem ms  = getParam Nothing "text" ms


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
           Just v   -> if typeOf v==typeOf "" then unsafeCoerce v else show v
        form= [finput tolook type1  nvalue False Nothing]
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    case getParam1 tolook env of
       Nothing ->  return $ FormElm form Nothing
       justx ->    return $ FormElm form justx

-- | return a new string, to be used in names of input field names
getNewName :: MonadState (MFlowState view) m =>  m String
getNewName= do
      st <- get
      let n= mfSequence st
      put $ st{mfSequence= n+1}
      return $  "p" ++ show n

getMultilineText :: (FormInput view,
      Monad m) =>
      Maybe String ->  View view m String
getMultilineText mt = View $ do
    tolook <- getNewName

    let nvalue= case mt of
           Nothing  ->  ""
           Just v ->  v
    env <- gets mfEnv
    let form= [ftextarea tolook nvalue]
    case getParam1 tolook env of
       Nothing -> return $ FormElm form Nothing
       justx  -> return $ FormElm form justx

instance  (MonadIO m, Functor m, FormInput view) => FormLet Bool m view where
   digest mv =  getBool b "True" "False"
       where
       b= case mv of
           Nothing -> Nothing
           Just bool -> Just $ show bool

getBool :: (FormInput view,
      Monad m) =>
      Maybe String -> String -> String -> View view m Bool
getBool mv truestr falsestr= View $  do
    tolook <- getNewName
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    case (getParam1 tolook env, mv) of
       (Nothing, Nothing) ->  return $ FormElm [foption1 tolook [truestr,falsestr] mv] Nothing
       (Nothing,Just x)   ->  return . FormElm [] . Just $ fromstr  x
       (Just x,_)         ->  return . FormElm [] . Just $ fromstr x
    where
    fromstr x= if x== truestr then True else False

getOption :: (FormInput view,
      Monad m) =>
      Maybe String ->[(String,String)] ->  View view m  String
getOption mv strings = View $ do
    tolook <- getNewName
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    case (getParam1 tolook env, mv) of
       (Nothing, Nothing) ->  return $ FormElm [foption tolook strings mv] Nothing
       (Nothing,Just x)   ->  return . FormElm [] $ Just  x
       (justx,_) -> return $ FormElm [] justx


-- | encloses instances of `Widget` or `FormLet` in formating
-- view is intended to be instantiated to a particular format
(<<<), wrap :: (Monad m, FormInput view, Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
wrap v form= View $ do
  FormElm f mx <- runView form 
  return $ FormElm [v $ mconcat f] mx

-- | encloses a widget in Html formatting
--
-- @table <<< (
--      tr <<< (td << widget widget1)
--      tr <<< (td << widget widget2))@

(<<<)= wrap
infixr 6 <<<



--
--instance (Monad m, ConvertTo v' v)
--      => ConvertTo (View v' m a) (View v m a) where
--   convert f= View $ do
--     FormElm fs mx <-  runView  f 
--     return $ FormElm (map convert fs ) mx


     
-- | append formatting to  `Widget` or `FormLet` instances
-- view is intended to be instantiated to a particular format
(<++) , addToForm :: (Monad m)
          => View v m a
          -> v
          -> View v m a 
addToForm form v= View $ do
  FormElm f mx <-  runView  form 
  return $ FormElm ( f ++ [ v]) mx 

  
infix 5 <++
-- | append formatting code to a widget
--
-- @ getString "hi" <++ H1 << "hi there"@

(<++)  = addToForm


infixr 5 ++>
-- | prepend formatting code to a widget
--
-- @bold << "enter name" ++> getString Nothing @

(++>) :: (Monad m, FormInput view, Monoid view)
       => view -> View view m a -> View view m a
html ++> digest =  (html `mappend`) <<< digest

-- | join two widgets in the same page
-- the resulting widget, when `ask`ed with it, returns a either one or the other
--
--  @r <- ask widget widget1 <+> widget widget2@
--



--instance(b ~ b') => Mix () b m b'  view where
--  (<+>) v w = widget $ v :<+> w
--instance(b ~ b') => Mix  b () m b'  view where
--  (<+>) v w = widget $ w :<+> v
--instance Mix a b n (Either a b) view where
--  (<+>) v w = widget $ v :<+> w
--

type Name= String
type Type= String
type Value= String
type Checked= Bool
type OnClick= Maybe String


-- | Minimal interface for defining the abstract basic form combinators
-- defined in this module. see "MFlow.Forms.XHtml" for the instance for "Text.XHtml"
-- format
class FormInput view where
    inred   :: view -> view
    fromString :: String -> view
    flink ::  String -> view -> view
    flink1:: String -> view
    flink1 verb = flink verb (fromString verb)
    finput :: Name -> Type -> Value -> Checked -> OnClick -> view
    ftextarea :: String -> String -> view
    foption :: String -> [(String,String)] -> Maybe String -> view
    foption1 :: String -> [String] -> Maybe String -> view
    foption1  name list msel= foption name (zip list list) msel
    formAction  :: String -> view -> view
    addAttributes :: view -> [(String,String)] -> view

-- | add attributes to the form element
-- if the view has more than one element, it is applied to  the first
infix 8 <!
widget <! atrs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  [addAttributes (head fs) atrs] mx


-------------------------------



instance (MonadIO m, Functor m, FormInput view)
         => FormLet User m view where
       digest muser=
        (User <$>  getString ( userName <$> muser)
              <*>  getPassword)
        `validate` userValidate


newtype Lang= Lang String

data MFlowState view= MFlowState{
   mfSequence :: Int,
   prevSeq    :: [Int],
   mfGoingBack :: Bool,
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


--rAnonUser= getDBRef . key $ eUser{userName=anonymous} :: DBRef User

mFlowState0 :: (FormInput view, Monoid view) => MFlowState view
mFlowState0= MFlowState 0 [] False  (Lang "en") [] False False (error "token of mFlowState0 used") 0 0 [] stdHeader False

setHeader :: Monad m => (view -> view) -> FlowM view m ()
setHeader header= do
  fs <- get
  put fs{mfHeader= header}

-- | set an HTTP cookie
setCookie :: MonadState (MFlowState view) m
          => String  -- ^ name
          -> String  -- ^ value
          -> String  -- ^ path
          -> Maybe String   -- ^ expires
          -> m ()
setCookie n v p me= do
    modify $ \st-> st{mfCookies=  (n,v,p,me):mfCookies st }

setTimeouts :: Monad m => Int -> Integer -> FlowM view m ()
setTimeouts kt st= do
 fs <- get
 put fs{ mfkillTime= kt, mfSessionTime= st}


getWFName ::   MonadState (MFlowState view) m =>   m String
getWFName = do
 fs <- get
 return . twfname $ mfToken fs

getCurrentUser ::  MonadState (MFlowState view) m=>  m String
getCurrentUser = return . tuser =<< gets mfToken



-- | Is an example of login\/register validation form needed by getUserWidget. In this case
-- the form field appears in a single line. it shows, in sequence, entries form user,
-- password, a button for loging, a entry to repeat password necesary for registering
-- and a button for registering.
-- The user can build its own user login\/validation forms by modifying this example
--
-- @ formUserLine=
--     (User \<\$\> getString (Just \"enter user\") \<\*\> getPassword \<\+\> submitButton \"login\")
--     \<\+\> fromString \"  password again\" \+\> getPassword \<\* submitButton \"register\"
-- @
formUserLine :: (FormInput view, Monoid view, Functor m, Monad m)
            => View view m (Maybe(Maybe User, Maybe String), Maybe String)
formUserLine=
       (User <$> getString (Just "enter user")
             <*> getPassword <+> submitButton "login")
       <+> fromString "  password again" ++> getPassword
           <*  submitButton "register"


-- | It creates a widget for user login\/registering. If a user name is specified
-- in the first parameter, it is forced to login\/password as this specific user
-- .Otherwise, if the user is already logged, the widget does not appear
userWidget :: ( MonadIO m, Functor m
                  , FormInput view, Monoid view )
                 => Maybe String
                 -> View view m (Maybe(Maybe User, Maybe String), Maybe String)
                 -> View view m String
userWidget muser formuser= View $ do
   rus <-  return . tuser =<< gets mfToken
   case  rus ==  anonymous of
     False -> getUser1w muser $ Just rus
     True   -> do
       env <- gets mfEnv
       getUser1w muser $ lookup cookieuser  env
                  
   where

   getUser1w mu (Just usname)=
      if isNothing mu || isJust mu && fromJust mu == usname
        then return $ FormElm [] $ Just usname
        else getUser1w Nothing Nothing

   getUser1w mu Nothing= do
      FormElm form mus <- runView $ wform  formuser `validate` val mu
      case mus of
       Nothing ->  return . FormElm form $ Nothing
       Just _  ->   do
          us <- getCurrentUser
          return . FormElm form $ Just us

--   val :: Maybe String
--       -> (Maybe(Maybe User, Maybe String), Maybe String)
--       -> WState view m (Maybe String)
   val _ (Nothing,_) = return $ Just ""
   val mu (Just (Just us , Just _), Nothing)=
        if isNothing mu || isJust mu && fromJust mu == userName us
           then login  us
           else return $ Just ""
   val mu (Just (Just us , Just _), Just p)=
      if isNothing mu || isJust mu && fromJust mu == userName us

        then  if  length p > 0 && upassword us== p
                  then do
                    userRegister' us
                    login  us

                  else return $ Just ""
        else return $ Just ""

--   login :: User -> WState view m (Maybe String)
   login  us=  do
     r <- userValidate us
     case r of
       Nothing -> do
         let uname= userName us
         st <- get
         let t = mfToken st
             t'= t{tuser= uname}
         moveState (twfname t) t t'
         put st{mfToken= t'}
         setCookie cookieuser uname "/" Nothing   -- !> "setcookie"
         return Nothing -- $ FormElm [fromString $ "user:"++uname] $ Just ()

       just -> return  just


-- | Very basic user authentication. The user is stored in a cookie.
-- it looks for the cookie. If no cookie, it ask to the user for a `userRegister`ed
-- user-password combination.
-- The user-password combination is only asked if the user has not logged already
-- otherwise, the stored username is returned.
getUser :: ( FormInput view, Monoid view, Typeable view
           , ConvertTo (HttpData view) display, Typeable display
           , MonadIO m, Functor m)
          => Maybe String
          -> View view m (Maybe(Maybe User, Maybe String), Maybe String)
          -> FlowM view m String
getUser mu form= ask $ userWidget mu form


instance   (MonadIO m, Functor m, m1 ~ m, b ~ a)
           => Widget(View view m1 b) a m view where
    widget =  id

-- | join two widgets in the same pages
-- the resulting widget, when `ask`ed with it, returns a either one or the other
(<+>) , mix ::  ( FormInput view , Monad m)
      => View view m a'
      -> View view m b'
      -> View view m (Maybe a', Maybe b')
mix digest1 digest2= View $ do
  FormElm f1 mx' <- runView  digest1
  FormElm f2 my' <- runView  digest2
  return $ FormElm (f1++f2) 
         $ case (mx',my') of
              (Nothing, Nothing) -> Nothing
              other              -> Just other

infixr 2 <+>

(<+>)  = mix


infix 3 <**, **>
-- | the first elem result (even if it is not validated) is discarded, and the secod is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the first elem fails.
-- . The first element is displayed however, as in the case of '*>'
(**>) :: (Functor m, Monad m)
      => View view m a -> View view m b -> View view m b
(**>) form1 form2 = valid form1 *> form2


-- | the second elem result (even if it is not validated) is discarded, and the first is returned
-- . This contrast with the applicative operator '<*' which fails the whole validation if
-- the validation of the first elem fails.
-- . The first element is displayed however, as in the case of '<*'
(<**)
  :: (Functor m, Monad m) =>
     View view m a -> View view m b -> View view m a
(<**) form1 form2 =  form1 <* valid form2


valid form= View $ do
   FormElm form mx <- runView form
   return $ FormElm form $ Just undefined



-- | it is the way to interact with the user.
-- It takes a widget and return the user result
-- If the environment has the result, ask don't ask to the user.
-- To force asking in any case, put an `clearEnv` statement before
-- in the FlowM monad
ask
  :: (Widget a b  m view,
      ConvertTo (HttpData view) display,
      FormInput view,
      Monoid view,
      Typeable view,
      Typeable display) =>
      a -> FlowM view m b
ask x = do
     st1 <-  get
     let st= st1{hasForm= False, needForm= False}
     put st                                   -- !>  ("before " ++ show (mfSequence st) ++ "level: " ++ show(length ( prevSeq st1)))
     FormElm forms mx <- lift $ runView $ widget x
     st' <- get
     case mx of 
       Just x -> do
         put st'{mfGoingBack=False
                ,prevSeq= mfSequence st: prevSeq st'}
         backPointReturn  x                    -- !> "just x"
       _ ->
         if mfGoingBack st && not (formMatch (mfSequence st) (mfSequence st') (mfEnv st'))
          then do
             put st'{mfSequence= head1 (prevSeq st')
                    ,mfGoingBack= True
                    ,prevSeq=tail1 (prevSeq st')}
             fail repeatPlease                         -- !> repeatPlease

          else do
             let header= mfHeader st'
                 t= mfToken st1
                 cont = case (needForm st', hasForm st') of
                   (True, False) -> header $ formAction (twfname t) $ mconcat forms
                   _    ->  header $ mconcat  forms 
             liftIO . sendFlush t $ HttpData (mfCookies st') cont
             put st{mfCookies=[]}                   -- !> ("after "++show ( mfSequence st'))
             receiveWithTimeouts
             st'' <- get
             if  not $ formMatch (mfSequence st) (mfSequence st') (mfEnv st'')
              then do
                 put st''{mfSequence= head1 (prevSeq st'')
                        ,mfGoingBack= True
                        ,prevSeq=tail1 (prevSeq st'')}
                 fail repeatPlease                  -- !> repeatPlease
              else ask x
    where
    head1 [] = 0
    head1 xs= head xs
    tail1 []= []
    tail1 xs= tail xs
    formMatch s s' env
       | s ==  s' = True
       | otherwise= (not . null) (filter (isJust . getparm env) [s .. s'-1])

    getparm env i= lookup ("p" ++ show i) env




--formExec
--  :: Widget a1 a m view =>
--     a1 -> FlowM view m (FormElm view a, MFlowState view)
--
--formExec x = do
--     st <-  get
--     let st= st{hasForm= False, needForm= False}
--     put st
--
--     lift $ runStateT ( runView $ widget x) st



-- | clears the environment
clearEnv :: Monad m => FlowM view m ()
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


data Selection a view= Selection{stitle:: view, sheader :: [view] , sbody :: [([view],a)]}


wform ::  (FormInput view, Monoid view, Widget a b m view)
          => a -> View view m b

wform x = View $ do
         FormElm form mr <- (runView $ widget  x )
         st <- get
         let t = mfToken  st
         put st{hasForm= True}
         let form1= formAction (twfname t) $ mconcat form

         return $ FormElm [form1] mr

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


--data Link a view  = Link a view

wlink :: (Show a, Read a, Typeable a, MonadIO m, Functor m, FormInput view)
         => a -> view -> View  view m a
wlink x v= View $ do
      verb <- getWFName
      name <- getNewName
      env <- gets mfEnv
      let mn = getParam1 name env
          showx= if typeOf x== typeOf "" then unsafeCoerce x else show x
          toSend = flink (verb ++ "?" ++name++"="++ showx) v
      return $ FormElm [toSend] mn 




instance Widget a b m view => Widget [a] b m view where
  widget xs = View $ do
      forms <- mapM(\x -> (runView  $  widget x )) xs
      let vs= concatMap (\(FormElm v _) -> v) forms
          res=  filter isJust $ map (\(FormElm _ r) -> r) forms
          res1= if null res then Nothing else head res
      return $ FormElm vs res1 




(|*>),wintersperse :: (MonadIO m, Functor m,Monoid view)
            => View view m r
            -> [View view m r']
            -> View view m (Maybe r,Maybe r')
wintersperse x xs= View $ do
  FormElm fxs rxs <-  runView $ widget  xs
  FormElm fx rx   <- runView $  x

  return $ FormElm (fx ++ intersperse (mconcat fx) fxs ++ fx)
         $ case (rx,rxs) of
            (Nothing, Nothing) -> Nothing
            other -> Just other

(|*>)= wintersperse

infix 5 |*>

(|+|) w w'= wintersperse w [w']

infix 1 |+|


-- | flatten a tree of tuples of Maybe results characteristic of the <+> operator
-- This is useful to make matching easier . For example:
--
-- @ res <- ask $ wlink1 <+> wlink2 wform <+> wlink3 <+> wlink4@
--
-- has type:
--
-- @Maybe (Maybe (Maybe (Maybe (Maybe a,Maybe b),Maybe c),Maybe d),Maybe e)@
--
-- but @flatten res@ has type:
--
-- @ (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e)

flatten :: Flatten (Maybe a) b => a -> b
flatten res= doflat $ Just res

class Flatten a b  where
 doflat :: a -> b


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

