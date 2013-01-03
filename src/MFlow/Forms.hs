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
>              <+> (fromStr "  password again" ++> getPassword  <! [("size","5")]
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
>    ajaxc <- ajax "document.getElementById('text1').value"
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
-- FormLet(..), 
FlowM, View(..), FormElm(..), FormInput(..)

-- * Users 
,userRegister, userValidate, isLogged, User(userName), setAdminUser, getAdminName
,getCurrentUser,getUserSimple, getUser, userFormLine, userLogin,logout, userWidget,getLang,
-- * User interaction 
ask, clearEnv, wstateless, transfer,
-- * formLets 
-- | they mimic the HTML form elements.
-- It is possible to modify their attributes with the `<!` operator.
-- They are combined with the widget combinators.
-- formatting can be added with the formatting combinators.
-- modifiers change their presentation and behaviour
getString,getInt,getInteger, getTextBox 
,getMultilineText,getBool,getSelect, setOption,setSelectedOption, getPassword,
getRadio, setRadio, setRadioActive, getCheckBoxes, genCheckBoxes, setCheckBox,
submitButton,resetButton, whidden, wlink, returning, wform, firstOf, manyOf, wraw, wrender
-- * FormLet modifiers
,validate, noWidget, waction, wmodify,

-- * Caching widgets
cachedWidget, wcached, wfreeze, 
-- * Widget combinators
(<+>),(|*>),(|+|), (**>),(<**),(<|>),(<*),(<$>),(<*>),(>:>)

-- * Normalized (convert to ByteString) widget combinators
-- | these dot operators are indentical to the non dot operators, with the addition of the conversion of the arguments to lazy byteStrings
--
-- The purpose is to combine heterogeneous formats into byteString-formatted widgets that
-- can be cached with `cachedWidget`
,(.<+>.), (.|*>.), (.|+|.), (.**>.),(.<**.), (.<|>.),

-- * Formatting combinators
(<<<),(<++),(++>),(<!),

-- * Normalized (convert to ByteString) formatting combinators
-- | some combinators that convert the formatting of their arguments to lazy byteString
(.<<.),(.<++.),(.++>.)

-- * ByteString tags
,btag,bhtml,bbody

-- * Normalization
, flatten, normalize

-- * Running the flow monad
,runFlow,runFlowIn,MFlow.Forms.Internals.step, goingBack,breturn

-- * Setting parameters
,setHeader
,setSessionData
,getSessionData
,getHeader
,setTimeouts

-- * Cookies
,setCookie
-- * Ajax
,ajax
,ajaxSend
,ajaxSend_
-- * Requirements
,Requirements(..)
,requires
-- * Utility
,genNewId
,changeMonad

,installServerControl
,getToken
,receiveWithTimeouts
,getEnv
,MFlowState
)
where

import Data.RefSerialize hiding ((<|>))
import Data.TCache
import Data.TCache.Memoization
import MFlow
import MFlow.Forms.Internals
import MFlow.Cookies
import Data.ByteString.Lazy.Char8 as B(ByteString,cons,pack,unpack,append,empty,fromChunks) 
import Data.List
import qualified Data.CaseInsensitive as CI
import Data.Typeable
import Data.Monoid
import Control.Monad.State.Strict 
import Data.Maybe
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Workflow as WF 
import Control.Monad.Identity
import Unsafe.Coerce
import Data.List(intersperse)
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import Data.Char(isNumber)
import Network.HTTP.Types.Header


import Debug.Trace
(!>)= flip trace



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
      modify (\s -> s{inSync= True})
      case me of
         Just str ->
           --FormElm form mx' <- generateForm [] (Just x) noValidate
           return $ FormElm ( inred (fromStr str) : form) Nothing
         Nothing  -> return $ FormElm [] mx
    _ -> return $ FormElm form mx

-- | Actions are callbacks that are executed when a widget is validated.
-- It is useful when the widget is inside widget containers that know nothing about his content.
--
-- It returns a result  that can be significative or, else, be ignored with '<**' and '**>'.
-- An action may or may not initiate his own dialog with the user via `ask`
waction
  :: (FormInput view, Monad m)
     => View view m a
     -> (a -> FlowM view m b)
     -> View view m b
waction f ac = do
  x <- f
  s <- get
  let env =  mfEnv s
  let seq = mfSequence s
  put s{mfSequence=mfSequence s+ 100,mfEnv=[]}
  r <- flowToView $ ac x !> "ACTION"
  modify $ \s-> s{mfSequence= seq, mfEnv= env}
  return r
  where
  flowToView x=
          View $ do
              r <- runBackT $ runFlowM  x
              case r of
                NoBack x ->
                     return (FormElm [] $ Just x)
                BackPoint x->
                     return (FormElm [] $ Just x)
                GoBack-> do
                     modify $ \s ->s{notSyncInAction= True}
                     return (FormElm [] Nothing)

wmodify :: (Monad m, FormInput v)
        => View v m a
        -> ([v] -> Maybe a -> WState v m ([v], Maybe b))
        -> View v m b
wmodify formt act = View $ do
   FormElm f mx <- runView  formt 
   (f',mx') <-  act f mx
   return $ FormElm f' mx'


--
--instance (FormInput view, FormLet a m view , FormLet b m view )
--          => FormLet (a,b) m view  where
--  digest  mxy  = do
--      let (x,y)= case mxy of Nothing -> (Nothing, Nothing); Just (x,y)-> (Just x, Just y)
--      (,) <$> digest x   <*> digest  y
--
--instance (FormInput view, FormLet a m view , FormLet b m view,FormLet c m view )
--          => FormLet (a,b,c) m view  where
--  digest  mxy  = do
--      let (x,y,z)= case mxy of Nothing -> (Nothing, Nothing, Nothing); Just (x,y,z)-> (Just x, Just y,Just z)
--      (,,) <$> digest x  <*> digest  y  <*> digest  z

-- | display a text box and return a String
getString  :: (FormInput view,Monad m) =>
     Maybe String -> View view m String
getString = getTextBox

-- | display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

-- | display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

-- | display a password box 
getPassword :: (FormInput view,
     Monad m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

data Radio= Radio String
-- | implement a radio button that perform a submit when pressed.
-- the parameter is the name of the radio group
setRadioActive :: (FormInput view,  MonadIO m) =>
             String -> String -> View view m  Radio
setRadioActive  v n = View $ do
  st <- get
  put st{needForm= True}
  let env =  mfEnv st
  FormElm form mn <- getParam1 n env []
  return $ FormElm [finput n "radio" v
          ( isJust mn  && v== fromJust mn) (Just  "this.form.submit()")]
          (fmap Radio mn)


-- | implement a radio button
-- the parameter is the name of the radio group
setRadio :: (FormInput view,  MonadIO m) =>
            String -> String -> View view m  Radio
setRadio v n= View $ do
      st <- get
      put st{needForm= True}
      let env =  mfEnv st
      FormElm f mn <- getParam1 n env []
      return $ FormElm
          (f++[finput n "radio" v
              ( isJust mn && v== fromJust mn) Nothing])
          (fmap Radio mn)

getRadio
  :: (Monad m, Functor m, FormInput view) =>
     [String -> View view m Radio] -> View view m String
getRadio rs=  do
        id <- genNewId
        Radio r <- firstOf $ map (\r -> r id)  rs
        return r

data CheckBoxes = CheckBoxes [String]

instance Monoid CheckBoxes where
  mappend (CheckBoxes xs) (CheckBoxes ys)= CheckBoxes $ xs ++ ys
  mempty= CheckBoxes []

--instance (Monad m, Functor m) => Monoid (View v m CheckBoxes) where
--  mappend x y=  mappend <$> x <*> y
--  mempty= return (CheckBoxes [])

instance (Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- ^ beware that both operands must validate to generate a sum
  mempty= return mempty

-- | display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (FormInput view,  MonadIO m) =>
                Bool -> String -> View view m  CheckBoxes
setCheckBox checked v= View $ do
  n <- genNewId
  st <- get
  put st{needForm= True}
  let env = mfEnv st
      strs= map snd $ filter ((==) n . fst) env
      mn= if null strs then Nothing else Just $ head strs

  val <- gets inSync
  let ret= case val of
        True ->  Just $ CheckBoxes  strs
        False -> Nothing
  return $ FormElm
      ( [ finput n "checkbox" v
        ( checked || (isJust mn  && v== fromJust mn)) Nothing])
      ret

-- | read the checkboxes dinamically created by JavaScript within the view parameter
-- see for example `selectAutocomplete` in "MFlow.Forms.Widgets"
genCheckBoxes :: (Monad m, FormInput view) => view ->  View view m  CheckBoxes
genCheckBoxes v= View $ do
  n <- genNewId
  st <- get
  put st{needForm= True}
  let env = mfEnv st
      strs= map snd $ filter ((==) n . fst) env
      mn= if null strs then Nothing else Just $ head strs

  val <- gets inSync
  let ret= case val of
        True ->  Just $ CheckBoxes  strs
        False -> Nothing
  return $ FormElm [ftag "span" v `attrs`[("id",n)]] ret

whidden :: (Monad m, FormInput v,Read a, Show a, Typeable a) => a -> View v m a
whidden x= View $ do
  n <- genNewId
  env <- gets mfEnv
  let showx= case cast x of
              Just x' -> x'
              Nothing -> show x
  getParam1 n env [finput n "hidden" showx False Nothing]

getCheckBoxes ::(FormInput view, Monad m)=> View view m  CheckBoxes -> View view m [String]
getCheckBoxes boxes =  View $ do
    n <- genNewId
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
getEnv ::  MonadState (MFlowState view) m =>  m Params
getEnv = gets mfEnv
     
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
       Nothing  -> genNewId  
       Just n -> return n
    let nvalue= case mvalue of
           Nothing  -> ""
           Just v   ->
               case cast v of
                 Just v' -> v'
                 Nothing -> show v
--             let typev= typeOf v
--             in if typev==typeOf (undefined :: String) then unsafeCoerce v
--                else if typev==typeOf (undefined :: String) then unsafeCoerce v
--                else if typev==typeOf (undefined :: ByteString) then unsafeCoerce v
--                else show v
        form= [finput tolook type1 nvalue False Nothing]
    st <- get
    let env = mfEnv st
    put st{needForm= True}
    getParam1 tolook env form
       
-- | generate a new string. Useful for creating tag identifiers and other attributes
genNewId :: MonadState (MFlowState view) m =>  m String
genNewId=  do
  st <- get
  case mfCached st of
    False -> do
      let n= mfSequence st
      put $ st{mfSequence= n+1}
      return $ 'p':(show n)
    True  -> do
      let n = mfSeqCache st
      put $ st{mfSeqCache=n+1}
      return $  'c' : (show n)






--genNewId :: MonadState (MFlowState view) m =>  m String
--genNewId= do
--      st <- get
--      let n= mfSequence st
--      put $ st{mfSequence= n+1}
--      let pref= if mfCached st then 'c' else 'p'
--      return $  pref : (show n)

getCurrentName :: MonadState (MFlowState view) m =>  m String
getCurrentName= do
     st <- get
     let parm = mfSequence st
     return $ "p"++show parm


-- | display a multiline text box and return its content
getMultilineText :: (FormInput view,
      Monad m) =>
      String ->  View view m String
getMultilineText nvalue = View $ do
    tolook <- genNewId
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
    tolook <- genNewId
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
    tolook <- genNewId
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




-- | add attributes to the form element
-- if the view has more than one element, it is applied to  the first
infix 8 <!
widget <! atrs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  [attrs (head fs) atrs] mx


-------------------------------

--
--
--instance (MonadIO m, Functor m, FormInput view)
--         => FormLet User m view where
--       digest muser=
--        (User <$>  getString ( userName <$> muser)
--              <*>  getPassword)
--        `validate` userValidate






-- | Is an example of login\/register validation form needed by 'userWidget'. In this case
-- the form field appears in a single line. it shows, in sequence, entries for the username,
-- password, a button for loging, a entry to repeat password necesary for registering
-- and a button for registering.
-- The user can build its own user login\/validation forms by modifying this example
--
-- @ userFormLine=
--     (User \<\$\> getString (Just \"enter user\") \<\*\> getPassword \<\+\> submitButton \"login\")
--     \<\+\> fromStr \"  password again\" \+\> getPassword \<\* submitButton \"register\"
-- @
userFormLine :: (FormInput view, Functor m, Monad m)
            => View view m (Maybe (UserStr,PasswdStr), Maybe PasswdStr)
userFormLine=
       ((,)  <$> getString (Just "enter user")                  <! [("size","5")]
             <*> getPassword                                    <! [("size","5")]
         <** submitButton "login")
         <+> (fromStr "  password again" ++> getPassword      <! [("size","5")]
         <*  submitButton "register")

-- | Example of user\/password form (no validation) to be used with 'userWidget'
userLogin :: (FormInput view, Functor m, Monad m)
            => View view m (Maybe (UserStr,PasswdStr), Maybe String)
userLogin=
        ((,)  <$> fromStr "Enter User: " ++> getString Nothing     <! [("size","4")]
              <*> fromStr "  Enter Pass: " ++> getPassword         <! [("size","4")]
              <** submitButton "login")
              <+> (noWidget
              <*  noWidget)



-- | empty widget that return Nothing. May be used as \"empty boxes\" inside larger widgets
noWidget ::  (FormInput view,
     Monad m) =>
     View view m a
noWidget= View . return $ FormElm  [] Nothing

-- | render a  value and return it
wrender
  :: (Monad m, Functor m, Show a, FormInput view) =>
     a -> View view m a
wrender x = (wraw . fromStr $ show x) **> return x

-- | render raw view formatting. It is useful for displaying information
wraw :: Monad m => view -> View view m ()
wraw x= View . return . FormElm [x] $ Just ()


-- | Wether the user is logged or is anonymous
isLogged :: MonadState (MFlowState v) m => m Bool
isLogged= do
   rus <-  return . tuser =<< gets mfToken
   return . not $ rus ==  anonymous

-- | It creates a widget for user login\/registering. If a user name is specified
-- in the first parameter, it is forced to login\/password as this specific user.
-- If this user was already logged, the widget return the user without asking.
-- If the user press the register button, the new user-password is registered and the
-- user logged.
userWidget :: ( MonadIO m, Functor m
          , FormInput view) 
         => Maybe String
         -> View view m (Maybe (UserStr,PasswdStr), Maybe String)
         -> View view m String
userWidget muser formuser= do
   user <- getCurrentUser
   if muser== Just user
         then return user
         else formuser `validate` val muser `waction` login 
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

--   val _ _ = return $ Just "Please fill in the fields for login or register"

   login (Just (u,p), Nothing)= do
         let uname= u
         st <- get
         let t = mfToken st
             t'= t{tuser= uname}
         moveState (twfname t) t t'
         put st{mfToken= t'}
         liftIO $ deleteTokenInList t
         liftIO $ addTokenToList t'
         setCookie cookieuser   uname "/"  (Just $ 365*24*60*60) 
         return uname

   login (Just us@(u,p), Just _)=  do
         userRegister u p
         login (Just us , Nothing)

-- | logout. The user is resetted to the `anonymous` user
logout :: (MonadIO m, MonadState (MFlowState view) m) => m ()
logout= do
     st <- get
     let t = mfToken st
         t'= t{tuser= anonymous}
     moveState (twfname t) t t'
     put st{mfToken= t'}
     liftIO $ deleteTokenInList t
     liftIO $ addTokenToList t'

     setCookie cookieuser   anonymous "/" (Just $ -1000)

-- | If not logged, perform login. otherwise return the user
--
-- @getUserSimple= getUser Nothing userFormLine@
getUserSimple :: ( FormInput view, Typeable view
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


infixr 1  **> , .**>. ,  <** , .<**.
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
      FormInput view,
      MonadIO m,
      Typeable view) =>
      View view m b -> FlowM view m b
ask w =  do
  st1 <- get
  let env= mfEnv st1
  case (mfAjax st1,lookup "ajax" env, lookup "val" env)  of
   ( Just ajaxl,Just v1, Just v2) -> do
     let f = fromMaybe (error $ "not found Ajax handler for: "++ v1) $ M.lookup v1 ajaxl
     FlowM . lift $ (unsafeCoerce f) v2
     FlowM $ lift receiveWithTimeouts
     ask w

   _ ->   do
     let st= st1{needForm= False, inSync= False, mfRequirements= []} 
     put st
     FormElm forms mx <- FlowM . lift $ runView  w
              
     st' <- get
     if notSyncInAction st' then put st'{notSyncInAction=False}>> ask w  else
      case mx    of

       Just x -> do
         put st'{prevSeq= mfSequence st: prevSeq st',onInit= True ,mfEnv=[]}
         breturn x -- BackT . return $ BackPoint  x                                 -- !> "just x"

       Nothing ->
         if  not (inSync st') && not (onInit st') && hasParams (mfSequence st') (mfSeqCache st') ( mfEnv st')  -- !> (show $ inSync st')  !> (show $ onInit st')
          then do
             put st'{mfSequence= head1 $ prevSeq st'
                    ,prevSeq= tail1 $ prevSeq st' }
             fail ""
          else do
             reqs <-  FlowM $ lift installAllRequirements
             let header= mfHeader st'
                 t= mfToken st'
                 cont = case (needForm st') of
                      True ->  header $  reqs <> (formAction (twfname t ) $ mconcat forms)
                      _    ->  header $  reqs <> mconcat  forms

                 HttpData ctype c s= toHttpData cont 
             liftIO . sendFlush t $ HttpData (ctype++mfHttpHeaders st') (mfCookies st' ++ c) s
             put st{mfCookies=[],mfHttpHeaders=[], onInit= False, mfToken= t, mfAjax= mfAjax st', mfSeqCache= mfSeqCache st' }                --    !> ("after "++show ( mfSequence st'))
             FlowM $ lift  receiveWithTimeouts
             ask w
    where
    head1 []=0
    head1 xs= head xs
    tail1 []=[]
    tail1 xs= tail xs

    hasParams seq cseq= not . null . filter (\(p,_) ->
       let tailp = tail p
       in  and (map isNumber tailp) &&
       let rt= read tailp
       in  case head p of
         'p' -> rt <= seq
         'c' -> rt <= cseq
         _   -> False)
--       (head p== 'p' || head p == 'c')
--       && and (map isNumber tailp)
--       && read  tailp <= seq)

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
    return $ not (inSync st) && not (onInit st)

-- | Clears the environment
clearEnv :: MonadState (MFlowState view) m =>  m ()
clearEnv= do
  st <- get
  put st{ mfEnv= []}

receiveWithTimeouts :: MonadIO m => WState view m ()
receiveWithTimeouts= do
         st <- get
         let t= mfToken st
             t1= mfkillTime st
             t2= mfSessionTime st
         req <- return . getParams =<< liftIO ( receiveReqTimeout t1 t2  t)
         put st{mfEnv= req}

-- | it creates a stateless flow (see `stateless`) whose behaviour is defined as a widget  
wstateless
  :: (Typeable view,  FormInput view) =>
     View view IO a -> Flow
wstateless w = transient $ runFlow loop
  where
  loop= do
      ask w
      env <- get
      put $ env{ mfSequence= 0,prevSeq=[]} 
      loop

---- | it creates a stateless flow (see `stateless`) whose behaviour is defined as a widget  
----
---- This version writes a log with all the values returned by ask
--wstatelessLog
--  :: (Typeable view, ToHttpData view, FormInput view,Serialize a,Typeable a) =>
--     View view IO a -> (Token -> Workflow IO ())
--wstatelessLog w = runFlow loop
--  where
--  loop= do
--      MFlow.Forms.step $ do
--         r <- ask w
--         env <- get
--         put $ env{ mfSequence= 0,prevSeq=[]}
--         return r
--      loop

-- | transfer control to another flow.
transfer :: MonadIO m => String -> FlowM v m ()
transfer flowname =do
         t <- gets mfToken
         let t'= t{twfname= flowname}
         liftIO  $ do
             (r,_) <- msgScheduler t'
             sendFlush t r



-- | wrap a widget of form element within a form-action element.
---- Usually it is done automatically by the @Wiew@ monad.
wform ::  (Monad m, FormInput view)
          => View view m b -> View view m b  

wform x = View $ do
         FormElm form mr <- (runView $   x )
         st <- get
         let t = mfToken  st
         anchor <- genNewId
         put st{needForm=False}
         let anchorf= (ftag "a") mempty  `attrs` [("name",anchor)]
         let form1= formAction (twfname t {-++"#"++anchor-}) $  mconcat ( anchorf:form)  -- !> anchor

         return $ FormElm [form1] mr

resetButton :: (FormInput view, Monad m) => String -> View view m () 
resetButton label= View $ return $ FormElm [finput  "reset" "reset" label False Nothing]   $ Just ()

submitButton :: (FormInput view, Monad m) => String -> View view m String
submitButton label= getParam Nothing "submit" $ Just label

newtype AjaxSessionId= AjaxSessionId String deriving Typeable

ajax :: (MonadIO m) => (String ->  View v m ByteString) ->  View v m (String -> String)
ajax  f =  do
     t <- gets mfToken
     id <- genNewId
     installServerControl id $ \x-> do
          setSessionData $ AjaxSessionId id
          r <- f x
          liftIO $ sendFlush t  (HttpData [("Content-Type", "text/plain")][] r )
          return ()

installServerControl :: MonadIO m => String -> (String -> View v m ()) -> View v m (String -> String)
installServerControl id f= do
      t <- gets mfToken
      st <- get
      let ajxl = fromMaybe M.empty $ mfAjax st
      let ajxl'= M.insert id (unsafeCoerce f ) ajxl
      put st{mfAjax=Just ajxl'}
      return $ \param ->  "doServer("++"'" ++  twfname t ++"','"++id++"',"++ param++")"


ajaxSend
  :: (Read a,MonadIO m) => View v m ByteString -> View v m a
ajaxSend cmd=  View $ do
   AjaxSessionId id <- getSessionData `onNothing` error "no AjaxSessionId set"
   env <- getEnv
   t <- getToken
   case (lookup "ajax" $ env, lookup "val" env) of
       (Nothing,_) -> return $ FormElm [] Nothing
       (Just id, Just _) -> do
           FormElm __ (Just  str) <- runView  cmd
           liftIO $ sendFlush t  $ HttpData [("Content-Type", "text/plain")][] $ str <>  readEvalLoop t id "''"
           receiveWithTimeouts
           env <- getEnv
           case (lookup "ajax" $ env,lookup "val" env) of
               (Nothing,_) -> return $ FormElm [] Nothing
               (Just id, Just v2) -> do
                    return $ FormElm []  . Just  $ read v2
   where
   readEvalLoop t id v = "doServer('"<> pack (twfname t)<>"','"<> pack id<>"',"<>v<>");" :: ByteString

ajaxSend_
  :: MonadIO m => View v m ByteString -> View v m ()
ajaxSend_ = ajaxSend


    
-- | creates a link wiget. A link can be composed with other widget elements,
wlink :: (Typeable a, Read a, Show a, MonadIO m, Functor m, FormInput view) 
         => a -> view -> View  view m a
wlink x v= View $ do
      verb <- getWFName
      name <- genNewId
      env  <- gets mfEnv 
      let
          showx= if typeOf x== typeOf (undefined :: String) then unsafeCoerce x else show x
          toSend = flink (verb ++ "?" ++  name ++ "=" ++ showx) v
      getParam1 name env [toSend]

-- | when some HTML produces a parameter in response, but it is not produced by
-- a form or a link, but for example by an script, returning notify the type checker
-- and the parameter extractor about this fact.
--
-- . The parameter is the visualization code, that accept a serialization function that generate
-- the server invocation string used by the visualization to return the value by means
-- of a link or a @window.location@ statement in javasCript
returning ::(Typeable a, Read a, Show a,Monad m, FormInput view) 
         => ((a->String) ->view) -> View view m a
returning expr=View $ do
      verb <- getWFName
      name <- genNewId
      env  <- gets mfEnv
      let string x=
            let showx= case cast x of
                   Just x' -> x'
                   _       -> show x
            in (verb ++ "?" ++  name ++ "=" ++ showx)
          toSend= expr string
      getParam1 name env [toSend]
      




--instance (Widget a b m view, Monoid view) => Widget [a] b m view where
--  widget xs = View $ do
--      forms <- mapM(\x -> (runView  $  widget x )) xs
--      let vs  = concatMap (\(FormElm v _) -> v) forms
--          res = filter isJust $ map (\(FormElm _ r) -> r) forms
--          res1= if null res then Nothing else head res
--      return $ FormElm [mconcat vs] res1

-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (Monoid view, Monad m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= View $ do 
      forms <- mapM runView  xs
      let vs  = concatMap (\(FormElm v _) ->  [mconcat v]) forms
          res = filter isJust $ map (\(FormElm _ r) -> r) forms
          res1= if null res then Nothing else head res
      return $ FormElm  vs res1

manyOf :: (FormInput view, MonadIO m, Functor m)=> [View view m a]  -> View view m [a]
manyOf xs= whidden () *> (View $ do 
      forms <- mapM runView  xs
      let vs  = concatMap (\(FormElm v _) ->  [mconcat v]) forms
          res1= catMaybes $ map (\(FormElm _ r) -> r) forms
      return $ FormElm  vs $ Just res1)

(>:>) ::(Monad m)=> View v m a -> View v m [a]  -> View v m [a]
(>:>) w ws= View $ do
    FormElm fs mxs <- runView $  ws
    FormElm f1 mx  <- runView w
    return $ FormElm (f1++ fs)
         $ case( mx,mxs) of
             (Just x, Just xs) -> Just $ x:xs
             (Nothing, mxs) -> mxs
             (Just x, _) -> Just [x]

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
(.<<.) :: (FormInput view) => (ByteString -> ByteString) -> view -> ByteString
(.<<.) w x = w ( toByteString x)

-- | > (.<+>.) x y = normalize x <+> normalize y
(.<+>.)
  :: (Monad m, FormInput v, FormInput v1) =>
     View v m a -> View v1 m b -> View ByteString m (Maybe a, Maybe b)
(.<+>.) x y = normalize x <+> normalize y

-- | > (.|*>.) x y = normalize x |*> map normalize y
(.|*>.)
  :: (Functor m, MonadIO m, FormInput v, FormInput v1) =>
     View v m r
     -> [View v1 m r'] -> View ByteString m (Maybe r, Maybe r')
(.|*>.) x y = normalize x |*> map normalize y

-- | > (.|+|.) x y = normalize x |+| normalize y
(.|+|.)
  :: (Functor m, MonadIO m, FormInput v, FormInput v1) =>
     View v m r -> View v1 m r' -> View ByteString m (Maybe r, Maybe r')
(.|+|.) x y = normalize x |+| normalize y

-- | > (.**>.) x y = normalize x **> normalize y
(.**>.)
  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
     View v m a -> View v1 m b -> View ByteString m b
(.**>.) x y = normalize x **> normalize y

-- | > (.<**.) x y = normalize x <** normalize y
(.<**.)
  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
     View v m a -> View v1 m b -> View ByteString m a
(.<**.) x y = normalize x <** normalize y

-- | > (.<|>.) x y= normalize x <|> normalize y
(.<|>.)
  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
     View v m a -> View v1 m a -> View ByteString m a
(.<|>.) x y= normalize x <|> normalize y

-- | > (.<++.) x v= normalize x <++ toByteString v
(.<++.) :: (Monad m, FormInput v, FormInput v') => View v m a -> v' -> View ByteString m a
(.<++.) x v= normalize x <++ toByteString v

-- | > (.++>.) v x= toByteString v ++> normalize x
(.++>.) :: (Monad m, FormInput v, FormInput v') => v -> View v' m a -> View ByteString m a
(.++>.) v x= toByteString v ++> normalize x


instance FormInput  ByteString  where
    toByteString= id
    toHttpData = HttpData [contentHtml ] []
    ftag x= btag x []
    inred = btag "b" [("style", "color:red")]
    finput n t v f c= btag "input"  ([("type", t) ,("name", n),("value",  v)] ++ if f then [("checked","true")]  else []
                              ++ case c of Just s ->[( "onclick", s)]; _ -> [] ) ""
    ftextarea name text= btag "textarea"  [("name", name)]   $ pack text

    fselect name   options=  btag "select" [("name", name)]   options

    foption value content msel= btag "option" ([("value",  value)] ++ selected msel)   content
            where
            selected msel = if  msel then [("selected","true")] else []

    attrs = addAttrs


    formAction action form = btag "form" [("action", action),("method", "post")]  form
    fromStr = pack
    fromStrNoEncode= pack

    flink  v str = btag "a" [("href",  v)]  str



