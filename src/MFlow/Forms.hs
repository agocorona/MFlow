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
             -XTemplateHaskell
             -XNoMonomorphismRestriction

#-}

{- |
MFlow run stateful server processes. This version is the first stateful web framework
that is as RESTful as a web framework can be.

The routes are expressed as normal, monadic haskell code in the FlowM monad. Local links
point to alternative routes within this monadic computation just like a textual menu
in a console application. Any GET page is directly reachable by means of a RESTful URL.

At any moment the flow can respond to the back button or to any RESTful path that the user may paste in the navigation bar.
If the procedure is waiting for another different page, the FlowM monad backtrack until the path partially match
. From this position the execution goes forward until the rest of the path match. This way the
statelessness is optional. However, it is possible to store a session state, which may backtrack or
not when the navigation goes back and forth. It is upto the programmer.


All the flow of requests and responses are coded by the programmer in a single procedure.
Allthoug single request-response flows are possible. Therefore, the code is
more understandable. It is not continuation based. It uses a log for thread state persistence and backtracking for
handling the back button. Back button state syncronization is supported out-of-the-box

The MFlow architecture is scalable, since the state is serializable and small

The processes are stopped and restarted by the
application server on demand, including the execution state (if the Wokflow monad is used).
Therefore session management is automatic. State consistence and transactions are given by the TCache package.

The processes interact trough widgets, that are an extension of formlets with
additional applicative combinators, formatting, link management, callbacks, modifiers, caching,
byteString conversion and AJAX. All is coded in pure haskell.

The interfaces and communications are abstract, but there are bindings for blaze-html, HSP, Text.XHtml and byteString
, Hack and WAI but it can be extended to non Web based architectures.

Bindings for hack, and hsp >= 0.8,  are not compiled by Hackage, and do not appear, but are included in the package files.
To use them, add then to the exported modules and execute cabal install

It is designed for applications that can be run with no deployment with runghc in order
to speed up the development process. see <http://haskell-web.blogspot.com.es/2013/05/a-web-application-in-tweet.html>

This module implement  stateful processes (flows) that are optionally persistent.
This means that they automatically store and recover his execution state. They are executed by the MFlow app server.
defined in the "MFlow" module.

These processses interact with the user trough user interfaces made of widgets (see below) that return back statically typed responses to
the calling process. Because flows are stateful, not request-response, the code is more understandable, because
all the flow of request and responses is coded by the programmer in a single procedure in the FlowM monad. Allthoug
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

* NEW IN THIS RELEASE

[@Runtime templates@]  'template', 'edTemplate', 'witerate' and 'dField' permit the edition of
the widget content at runtime, and the management of placeholders with input fields and data fields
within the template with no navigation in the client, little bandwidth usage and little server load. Enven less
than using 'autoRefresh'.

* IN PREVIOUS RELEASES

{@AutoRefresh@] Using `autoRefresh`, Dynamic widgets can refresh themselves with new information without forcing a refresh of the whole page

[@Push@]  With `push` a widget can push new content to the browser when something in the server happens

[@Error traces@] using the monadloc package, now each runtime error (in a monadic statement) has a complete execution trace.


[@RESTful URLs@] Now each page is directly reachable by means of a intuitive, RESTful url, whose path is composed by the sucession
of links clicked to reach such page and such point in the procedure. Just what you would expect.

[@Page flows@] each widget-formlet can have its own independent behaviour within the page. They can
refresh independently trough AJAX by means of 'autoRefresh'. Additionally, 'pageFlow' initiates the page flow mode or a
subpage flow by adding a well know indetifier prefix for links and form parameters.

[@Modal Dialogs@] 'wdialog' present a widget within a modal or non modal jQuery dialog. while a monadic
widget-formlet can add different form elements depending on the user responses, 'wcallback' can
substitute the widget by other. (See 'Demos/demos.blaze.hs' for some examples)

[@JQuery widgets@] with MFlow interface: 'getSpinner', 'datePicker', 'wdialog'

[@WAI interface@] Now MFlow works with Snap and other WAI developments. Include "MFlow.Wai" or "MFlow.Wai.Blaze.Html.All" to use it.

[@blaze-html support@] see <http://hackage.haskell.org/package/blaze-html> import "MFlow.Forms.Blaze.Html" or "MFlow.Wai.Blaze.Html.All" to use Blaze-Html

[@AJAX@] Now an ajax procedures (defined with 'ajax' can perform many interactions with the browser widgets, instead
of a single request-response (see 'ajaxSend').

[@Active widgets@] "MFlow.Forms.Widgets" contains active widgets that interact with the
server via Ajax and dynamically control other widgets: 'wEditList', 'autocomplete' 'autocompleteEdit' and others.

[@Requirements@] a widget can specify javaScript files, JavasScript online scipts, CSS files, online CSS and server processes
 and any other instance of the 'Requrement' class. See 'requires' and 'WebRequirements'

[@content-management@] for templating and online edition of the content template. See 'tFieldEd' 'tFieldGen' and 'tField'

[@multilanguage@] see 'mField' and 'mFieldEd'

[@URLs to internal states@] if the web navigation is trough GET forms or links,
 an URL can express a direct path to the n-th step of a flow, So this URL can be shared with other users.
Just like in the case of an ordinary stateless application.


[@Back Button@] This is probably the first implementation in any language where the navigation
can be expressed procedurally and still it works well with the back button, thanks
to monad magic. (See <http://haskell-web.blogspot.com.es/2012/03//failback-monad.html>)


[@Cached widgets@] with `cachedWidget` it is possible to cache the rendering of a widget as a ByteString (maintaining type safety)
, the caching can be permanent or for a certain time. this is very useful for complex widgets that present information. Specially if
the widget content comes from a database and it is  shared by all users.


[@Callbacks@] `waction` add a callback to a widget. It is executed when its input is validated.
The callback may initate a flow of interactions with the user or simply executes an internal computation.
Callbacks are necessary for the creation of abstract container
widgets that may not know the behaviour of its content. with callbacks, the widget manages its content as  black boxes.


[@Modifiers@] `wmodify` change the visualization and result returned by the widget. For example it may hide a
login form and substitute it by the username if already logged.

Example:

@ ask $ wform userloginform \``validate`\` valdateProc \``waction`\` loginProc \``wmodify`\` hideIfLogged@


[@attributes for formLet elements@]  to add atributes to widgets. See the  '<!' opèrator


[@ByteString normalization and hetereogeneous formatting@] For caching the rendering of widgets at the
 ByteString level, and to permit many formatring styles
in the same page, there are operators that combine different formats which are converted to ByteStrings.
For example the header and footer may be coded in XML, while the formlets may be formatted using Text.XHtml.

[@File Server@] With file caching. See "MFlow.FileServer"


-}

module MFlow.Forms(

-- * Basic definitions
-- FormLet(..),
FlowM, View(..), FormElm(..), FormInput(..)

-- * Users
, Auth(..), userRegister, setAuthMethod, userValidate, isLogged, setAdminUser, getAdminName
,getCurrentUser,getUserSimple, getUser, userFormLine, userLogin,logout, paranoidLogout
,encryptedLogout, userWidget, paranoidUserWidget, encryptedUserWidget, login, paranoidLogin, encryptedLogin,
userName,
-- * User interaction
ask, page, askt, clearEnv, wstateless, pageFlow,
-- * formLets
-- | They usually produce the HTML form elements (depending on the FormInput instance used)
-- It is possible to modify their attributes with the `<!` operator.
-- They are combined with applicative ombinators and some additional ones
-- formatting can be added with the formatting combinators.
-- modifiers change their presentation and behaviour
getString,getInt,getInteger, getTextBox
,getMultilineText,getBool,getSelect, setOption,setSelectedOption, getPassword,
getRadio, setRadio, setRadioActive, wlabel, getCheckBoxes, genCheckBoxes, setCheckBox,
submitButton,resetButton, whidden, wlink, absLink, getKeyValueParam, fileUpload,
getRestParam, returning, wform, firstOf, manyOf, allOf, wraw, wrender, notValid
-- * FormLet modifiers
,validate, noWidget, stop, waction, wcallback, wmodify,

-- * Caching widgets
cachedWidget, wcached, wfreeze,

-- * Widget combinators
(<+>),(|*>),(|+|), (**>),(<**),(<|>),(<*),(<$>),(<*>),(>:>)

---- * Normalized (convert to ByteString) widget combinators
---- | These dot operators are indentical to the non dot operators, with the addition of the conversion of the arguments to lazy byteStrings
----
---- The purpose is to combine heterogeneous formats into byteString-formatted widgets that
---- can be cached with `cachedWidget`
--,(.<+>.), (.|*>.), (.|+|.), (.**>.),(.<**.), (.<|>.),

-- * Formatting combinators
,(<<<),(++>),(<++),(<!)

---- * Normalized (convert to ByteString) formatting combinators
---- | Some combinators that convert the formatting of their arguments to lazy byteString
----(.<<.),(.<++.),(.++>.)

-- * ByteString tags
,btag,bhtml,bbody

-- * Normalization
,flatten, normalize

-- * Running the flow monad
,runFlow, transientNav, runFlowOnce, runFlowIn
,runFlowConf,MFlow.Forms.Internals.step
-- * controlling backtracking
,goingBack,returnIfForward, breturn, preventGoingBack, compensate, onBacktrack, retry

-- * Setting parameters
,setHttpHeader
,setHeader
,addHeader
,getHeader
,setSessionData
,getSessionData
,getSData
,delSessionData
,setTimeouts

-- * Cookies
,setCookie
,setParanoidCookie
,setEncryptedCookie
-- * Ajax
,ajax
,ajaxSend
,ajaxSend_
-- * Requirements
,Requirements(..)
,WebRequirement(..)
,requires
-- * Utility
,getSessionId
,getLang
,genNewId
,getNextId
,changeMonad
,FailBack
,fromFailBack
,toFailBack

)
where

import Data.RefSerialize hiding ((<|>),empty)
import Data.TCache
import Data.TCache.Memoization
import MFlow
import MFlow.Forms.Internals
import MFlow.Cookies
import Data.ByteString.Lazy.Char8 as B(ByteString,cons,append,empty,fromChunks,unpack)
import Data.ByteString.Lazy.UTF8 hiding (length, take)
import qualified Data.String as S
import qualified Data.Text as T
import Data.Text.Encoding
import Data.List
--import qualified Data.CaseInsensitive as CI
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
import Data.Char(isNumber,toLower)
import Network.HTTP.Types.Header
import MFlow.Forms.Cache

-- | Validates a form or widget result against a validating procedure
--
-- @getOdd= getInt Nothing `validate` (\x -> return $ if mod x 2==0 then  Nothing else Just "only odd numbers, please")@
validate
  :: (FormInput view,  Monad m) =>
     View view m a
     -> (a -> WState view m (Maybe view))
     -> View view m a
validate  formt val= View $ do
   FormElm form mx <- runView  formt
   case mx of
    Just x -> do
      me <- val x
      modify (\s -> s{inSync= True})
      case me of
         Just str ->
           return $ FormElm ( form <> inred  str) Nothing
         Nothing  -> return $ FormElm form mx
    _ -> return $ FormElm form mx

-- | Actions are callbacks that are executed when a widget is validated.
-- A action may be a complete flow in the flowM monad. It takes complete control of the navigation
-- while it is executed. At the end it return the result to the caller and display the original
-- calling page.
-- It is useful when the widget is inside widget containers that may treat it as a black box.
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
  put s{mfSequence=mfSequence s+ 100,mfEnv=[],newAsk=True}
  r <- flowToView $ ac x
  modify $ \s-> s{mfSequence= seq, mfEnv= env}
  return r
  where
  flowToView x=
          View $ do
              r <- runSup $ runFlowM  x
              case r of
                NoBack x ->
                     return (FormElm mempty $ Just x)
                BackPoint x->
                     return (FormElm mempty $ Just x)
                GoBack-> do
                     modify $ \s ->s{notSyncInAction= True}
                     return (FormElm mempty Nothing)

-- | change the rendering and the return value of a page. This is superseeded by page flows.
wmodify :: (Monad m, FormInput v)
        => View v m a
        -> (v -> Maybe a -> WState v m (v, Maybe b))
        -> View v m b
wmodify formt act = View $ do
   FormElm f mx <- runView  formt
   (f',mx') <-  act f mx
   return $ FormElm f' mx'

-- | Display a text box and return a non empty String
getString  :: (FormInput view,Monad m) =>
     Maybe String -> View view m String
getString ms = getTextBox ms
     `validate`
     \s -> if null s then return (Just $ fromStr "")
                    else return Nothing

-- | Display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

-- | Display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

-- | Display a password box
getPassword :: (FormInput view,
     Monad m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

newtype Radio a= Radio a

-- | Implement a radio button that perform a submit when pressed.
-- the parameter is the name of the radio group
setRadioActive :: (FormInput view,  MonadIO m,
                   Read a, Typeable a, Eq a, Show a) =>
             a -> String -> View view m  (Radio a)
setRadioActive  v n = View $ do
  st <- get
  put st{needForm= HasElems }
  let env =  mfEnv st
  mn <- getParam1 n env
  let str = if typeOf v == typeOf(undefined :: String)
                   then unsafeCoerce v else show v
  return $ FormElm (finput n "radio" str
          ( isValidated mn  && v== fromValidated mn) (Just  "this.form.submit()"))
          (fmap Radio $ valToMaybe mn)


-- | Implement a radio button
-- the parameter is the name of the radio group
setRadio :: (FormInput view,  MonadIO m,
             Read a, Typeable a, Eq a, Show a) =>
            a -> String -> View view m  (Radio a)
setRadio v n= View $ do
  st <- get
  put st{needForm= HasElems}
  let env =  mfEnv st
  mn <- getParam1 n env
  let str = if typeOf v == typeOf(undefined :: String)
                   then unsafeCoerce v else show v
  return $ FormElm (finput n "radio" str
          ( isValidated mn  && v== fromValidated mn) Nothing)
          (fmap Radio $ valToMaybe mn)

-- | encloses a set of Radio boxes. Return the option selected
getRadio
  :: (Monad m, Functor m, FormInput view) =>
     [String -> View view m (Radio a)] -> View view m a
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


-- | Display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (FormInput view,  MonadIO m) =>
                Bool -> String -> View view m  CheckBoxes
setCheckBox checked v= View $ do
  n <- genNewId
  st <- get
  put st{needForm= HasElems}
  let env = mfEnv st
      strs= map snd $ filter ((==) n . fst) env
      mn= if null strs then Nothing else Just $ head strs
      val = inSync st
  let ret= case val of                    -- !> show val of
        True  -> Just $ CheckBoxes  strs  -- !> show strs
        False -> Nothing
  return $ FormElm
      ( finput n "checkbox" v
        ( checked || (isJust mn  && v== fromJust mn)) Nothing)
      ret

-- | Read the checkboxes dinamically created by JavaScript within the view parameter
-- see for example `selectAutocomplete` in "MFlow.Forms.Widgets"
genCheckBoxes :: (Monad m, FormInput view) => view ->  View view m  CheckBoxes
genCheckBoxes v= View $ do
  n <- genNewId
  st <- get
  put st{needForm= HasElems}
  let env = mfEnv st
      strs= map snd $ filter ((==) n . fst) env
      mn= if null strs then Nothing else Just $ head strs

  val <- gets inSync
  let ret= case val of
        True ->  Just $ CheckBoxes  strs
        False -> Nothing
  return $ FormElm (ftag "span" v `attrs`[("id",n)]) ret

whidden :: (Monad m, FormInput v,Read a, Show a, Typeable a) => a -> View v m a
whidden x= View $ do
  n <- genNewId
  env <- gets mfEnv
  let showx= case cast x of
              Just x' -> x'
              Nothing -> show x
  r <- getParam1 n env
  return . FormElm (finput n "hidden" showx False Nothing) $ valToMaybe r

getCheckBoxes :: (FormInput view, Monad m)=> View view m  CheckBoxes -> View view m [String]
getCheckBoxes boxes =  View $ do
    n  <- genNewId
    st <- get
    let env =  mfEnv st
    let form= finput n "hidden" "" False Nothing
    mr  <- getParam1 n env

    let env = mfEnv st
    modify $ \st -> st{needForm= HasElems}
    FormElm form2 mr2 <- runView boxes
    return $ FormElm (form <> form2) $
        case (mr `asTypeOf` Validated ("" :: String),mr2) of
          (NoParam,_) ->  Nothing
          (Validated _,Nothing) -> Just []
          (Validated _, Just (CheckBoxes rs))  ->  Just rs





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
    let nvalue x = case x of
           Nothing  -> ""
           Just v   ->
               case cast v of
                 Just v' -> v'
                 Nothing -> show v
    st <- get
    let env = mfEnv st
    put st{needForm= HasElems}
    r <- getParam1 tolook env
    case r of
       Validated x        -> return $ FormElm (finput tolook type1 (nvalue $ Just x) False Nothing) $ Just x
       NotValidated s err -> return $ FormElm (finput tolook type1 s False Nothing <> err) $ Nothing
       NoParam            -> return $ FormElm (finput tolook type1 (nvalue mvalue) False Nothing) $ Nothing



--getCurrentName :: MonadState (MFlowState view) m =>  m String
--getCurrentName= do
--     st <- get
--     let parm = mfSequence st
--     return $ "p"++show parm


-- | Display a multiline text box and return its content
getMultilineText :: (FormInput view
                 ,  Monad m)
                   => T.Text
                 ->  View view m T.Text
getMultilineText nvalue = View $ do
    tolook <- genNewId
    env <- gets mfEnv
    r <- getParam1 tolook env
    case r of
       Validated x        -> return $ FormElm (ftextarea tolook  x) $ Just x
       NotValidated s err -> return $ FormElm (ftextarea tolook  (T.pack s))  Nothing
       NoParam            -> return $ FormElm (ftextarea tolook  nvalue)  Nothing


--instance  (MonadIO m, Functor m, FormInput view) => FormLet Bool m view where
--   digest mv =  getBool b "True" "False"
--       where
--       b= case mv of
--           Nothing -> Nothing
--           Just bool -> Just $ case bool of
--                          True ->  "True"
--                          False -> "False"

-- | Display a dropdown box with the two values (second (true) and third parameter(false))
-- . With the value of the first parameter selected.
getBool :: (FormInput view,
      Monad m, Functor m) =>
      Bool -> String -> String -> View view m Bool
getBool mv truestr falsestr= do
   r <- getSelect $   setOption truestr (fromStr truestr)  <! (if mv then [("selected","true")] else [])
                  <|> setOption falsestr(fromStr falsestr) <! if not mv then [("selected","true")] else []
   if  r == truestr  then return True else return False



-- | Display a dropdown box with the options in the first parameter is optionally selected
-- . It returns the selected option.
getSelect :: (FormInput view,
      Monad m,Typeable a, Read a) =>
      View view m (MFOption a) ->  View view m  a
getSelect opts = View $ do
    tolook <- genNewId
    st <- get
    let env = mfEnv st
    put st{needForm= HasElems}
    r <- getParam1 tolook env
    setSessionData $ fmap MFOption $ valToMaybe r
    FormElm form mr <- (runView opts)

    return $ FormElm (fselect tolook  form)  $ valToMaybe r


newtype MFOption a= MFOption a deriving Typeable

instance (FormInput view,Monad m, Functor m) => Monoid (View view m (MFOption a)) where
  mappend =  (<|>)
  mempty = Control.Applicative.empty

-- | Set the option for getSelect. Options are concatenated with `<|>`
setOption
  :: (Monad m, Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setOption n v = do
  mo <- getSessionData
  case mo  of
   Nothing -> setOption1 n v False
   Just Nothing -> setOption1 n v False
   Just (Just (MFOption o)) -> setOption1 n v $   n == o

-- | Set the selected option for getSelect. Options are concatenated with `<|>`
setSelectedOption
  :: (Monad m, Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setSelectedOption n v= do
  mo <- getSessionData
  case mo of
   Nothing -> setOption1 n v True
   Just Nothing -> setOption1 n v True
   Just (Just o) -> setOption1 n v $   n == o


setOption1 :: (FormInput view,
      Monad m, Typeable a, Eq a, Show a) =>
      a -> view -> Bool ->  View view m  (MFOption a)
setOption1 nam  val check= View $ do
    st <- get
    let env = mfEnv st
    put st{needForm= HasElems}
    let n = if typeOf nam == typeOf(undefined :: String)
                   then unsafeCoerce nam
                   else show nam

    return . FormElm (foption n val check)  . Just $ MFOption nam

-- | upload a file to a temporary file in the server
--
-- The user can move, rename it etc.
fileUpload :: (FormInput view,
      Monad  m,Functor m) =>
      View view m (String
                  ,String
                  ,String
                  ) -- ^ ( original file, file type, temporal uploaded)
fileUpload=
  getParam Nothing "file" Nothing <** modify ( \st ->  st{mfFileUpload = True})



-- | Enclose Widgets within some formating.
-- @view@ is intended to be instantiated to a particular format
--
-- NOTE: It has a infix priority : @infixr 5@ less than the one of @++>@ and @<++@ of the operators, so use parentheses when appropriate,
-- unless the we want to enclose all the widgets in the right side.
-- Most of the type errors in the DSL are due to the low priority of this operator.
--
-- This is a widget, which is a table with some links. it returns an Int
--
-- > import MFlow.Forms.Blaze.Html
-- >
-- > tableLinks :: View Html Int
-- > table ! At.style "border:1;width:20%;margin-left:auto;margin-right:auto"
-- >            <<< caption << text "choose an item"
-- >            ++> thead << tr << ( th << b << text  "item" <> th << b << text "times chosen")
-- >            ++> (tbody
-- >                 <<< tr ! rowspan "2" << td << linkHome
-- >                 ++> (tr <<< td <<< wlink  IPhone (b << text "iphone") <++  td << ( b << text (fromString $ show ( cart V.! 0)))
-- >                 <|>  tr <<< td <<< wlink  IPod (b << text "ipad")     <++  td << ( b << text (fromString $ show ( cart V.! 1)))
-- >                 <|>  tr <<< td <<< wlink  IPad (b << text "ipod")     <++  td << ( b << text (fromString $ show ( cart V.! 2))))
-- >                 )
(<<<) :: (Monad m,  Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
(<<<) v form= View $ do
  FormElm f mx <- runView form
  return $ FormElm (v  f) mx


infixr 5 <<<






-- | Append formatting code to a widget
--
-- @ getString "hi" '<++' H1 '<<' "hi there"@
--
-- It has a infix prority: @infixr 6@ higher than '<<<' and most other operators.
(<++) :: (Monad m, Monoid v)
      => View v m a
      -> v
      -> View v m a
(<++) form v= View $ do
  FormElm f mx <-  runView  form
  return $ FormElm ( f <> v) mx

infixr 6  ++>
infixr 6 <++
-- | Prepend formatting code to a widget
--
-- @bold '<<' "enter name" '++>' 'getString' 'Nothing' @
--
-- It has a infix prority: @infixr 6@ higher than '<<<' and most other operators
(++>) :: (Monad m,  Monoid view)
       => view -> View view m a -> View view m a
html ++> w =  --  (html <>) <<< digest
 View $ do
  FormElm f mx <- runView w
  return $ FormElm (html  <>  f) mx



-- | Add attributes to the topmost tag of a widget
--
-- It has a fixity @infix 8@
infixl 8 <!
widget <! attribs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  (fs `attrs` attribs) mx -- (head fs `attrs` attribs:tail fs) mx
--      case fs of
--        [hfs] -> return $ FormElm  [hfs `attrs` attribs] mx
--        _ -> error $ "operator <! : malformed widget: "++ concatMap (unpack. toByteString) fs


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
        ((,) <$> getString (Just "enter user")                  <! [("size","5")]
             <*> getPassword                                    <! [("size","5")]
         <** submitButton "login")
         <+> (fromStr "  password again" ++> getPassword      <! [("size","5")]
         <** submitButton "register")

-- | Example of user\/password form (no validation) to be used with 'userWidget'
userLogin :: (FormInput view, Functor m, Monad m)
          => View view m (Maybe (UserStr,PasswdStr), Maybe String)
userLogin=
        ((,)  <$> fromStr "Enter User: " ++> getString Nothing     <! [("size","4")]
              <*> fromStr "  Enter Pass: " ++> getPassword         <! [("size","4")]
              <** submitButton "login")
              <+> (noWidget
              <*  noWidget)



-- | Empty widget that does not validate. May be used as \"empty boxes\" inside larger widgets.
--
-- It returns a non valid value.
noWidget ::  (FormInput view,
     Monad m, Functor m) =>
     View view m a
noWidget= Control.Applicative.empty

-- | a sinonym of noWidget that can be used in a monadic expression in the View monad does not continue
stop :: (FormInput view,
     Monad m, Functor m) =>
     View view m a
stop= Control.Applicative.empty

-- | Render a Show-able  value and return it
wrender
  :: (Monad m, Functor m, Show a, FormInput view) =>
     a -> View view m a
wrender x = (fromStr $ show x) ++> return x

-- | Render raw view formatting. It is useful for displaying information.
wraw :: Monad m => view -> View view m ()
wraw x= View . return . FormElm x $ Just ()

-- To display some rendering and return  a no valid value
notValid :: Monad m => view -> View view m a
notValid x= View . return $ FormElm x Nothing

-- | Wether the user is logged or is anonymous
isLogged :: MonadState (MFlowState v) m => m Bool
isLogged= do
   rus <-  return . tuser =<< gets mfToken
   return . not $ rus ==  anonymous

-- | return the result if going forward
--
-- If the process is backtraking, it does not validate,
-- in order to continue the backtracking
returnIfForward :: (Monad m, FormInput view,Functor m) => b -> View view m b
returnIfForward x = do
     back <- goingBack
     if back then noWidget else return x

-- | forces backtracking if the widget validates, because a previous page handle this widget response
-- . This is useful for recurrent cached widgets or `absLink`s that are present in multiple pages. For example
-- in the case of menus or common options. The active elements of this widget must be cached with no timeout.
retry :: Monad m => View v m a -> View v m ()
retry w = View $ do
    FormElm v mx <- runView w
    when (isJust mx) $ modify $ \st -> st{inSync = False}
    return $ FormElm  v Nothing

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
userWidget muser formuser = userWidget' muser formuser login1

-- | Uses 4 different keys to encrypt the 4 parts of a MFlow cookie.

paranoidUserWidget muser formuser = userWidget' muser formuser paranoidLogin1

-- | Uses a single key to encrypt the MFlow cookie.

encryptedUserWidget muser formuser = userWidget' muser formuser encryptedLogin1

userWidget' muser formuser login1Func = do
   user <- getCurrentUser
   if muser== Just user || isNothing muser && user/= anonymous
         then returnIfForward user
         else formuser `validate` val muser `wcallback` login1Func
   where
   val _ (Nothing,_) = return . Just $ fromStr "Plese fill in the user/passwd to login, or user/passwd/passwd to register"

   val mu (Just us, Nothing)=
        if isNothing mu || isJust mu && fromJust mu == fst us
           then userValidate us
           else return . Just $ fromStr "This user has no permissions for this task"

   val mu (Just us, Just p)=
      if isNothing mu || isJust mu && fromJust mu == fst us
        then  if  Data.List.length p > 0 && snd us== p
                  then return Nothing
                  else return . Just $ fromStr "The passwords do not match"
        else return . Just $ fromStr "wrong user for the operation"

--   val _ _ = return . Just $ fromStr "Please fill in the fields for login or register"

login1
  :: (MonadIO m, MonadState (MFlowState view) m) =>
     (Maybe (UserStr, PasswdStr), Maybe t) -> m UserStr
login1 uname = login1' uname login

paranoidLogin1 uname  = login1' uname paranoidLogin

encryptedLogin1 uname = login1' uname encryptedLogin

login1' (Just (uname,_), Nothing) loginFunc= loginFunc uname >> return uname
login1' (Just us@(u,p), Just _) loginFunc=  do  -- register button pressed
             userRegister u p
             loginFunc u
             return u

-- | change the user
--
-- It is supposed that the user has been validated

login uname = login' uname setCookie

paranoidLogin uname = login' uname setParanoidCookie

encryptedLogin uname = login' uname setEncryptedCookie

login'
  :: (Num a1, S.IsString a, MonadIO m,
      MonadState (MFlowState view) m) =>
     String -> (String -> String -> a -> Maybe a1 -> m ()) -> m ()
login' uname setCookieFunc = do
    back <- goingBack
    if back then return () else do
     st <- get
     let t = mfToken st
         u = tuser t
     when (u /= uname) $ do
         let t'= t{tuser= uname}
    --     moveState (twfname t) t t'
         put st{mfToken= t'}
         liftIO $ deleteTokenInList t
         liftIO $ addTokenToList t'
         setCookieFunc cookieuser   uname "/"  (Just $ 365*24*60*60)

logout = logout' setCookie

paranoidLogout = logout' setParanoidCookie

encryptedLogout = logout' setEncryptedCookie


-- | logout. The user is reset to the `anonymous` user
logout'
  :: (Num a1,S.IsString a, MonadIO m,
      MonadState (MFlowState view) m) =>
     (String -> [Char] -> a -> Maybe a1 -> m ()) -> m ()
logout' setCookieFunc = do
    public
    back <- goingBack
    if back then return () else do
     st <- get
     let t = mfToken st
         t'= t{tuser= anonymous}
     when (tuser t /= anonymous) $ do
--         moveState (twfname t) t t'
         put st{mfToken= t'}
--         liftIO $ deleteTokenInList t
         liftIO $ addTokenToList t'
         setCookieFunc cookieuser   anonymous "/" (Just $ -1000)

-- | If not logged, perform login. otherwise return the user
--
-- @getUserSimple= getUser Nothing userFormLine@
getUserSimple :: ( FormInput view, Typeable view)
              => FlowM view IO String
getUserSimple= getUser Nothing userFormLine

-- | Very basic user authentication. The user is stored in a cookie.
-- it looks for the cookie. If no cookie, it ask to the user for a `userRegister`ed
-- user-password combination.
-- The user-password combination is only asked if the user has not logged already
-- otherwise, the stored username is returned.
--
-- @getUser mu form= ask $ userWidget mu form@
getUser :: ( FormInput view, Typeable view)
          => Maybe String
          -> View view IO (Maybe (UserStr,PasswdStr), Maybe String)
          -> FlowM view IO String
getUser mu form= ask $ userWidget mu form

-- | Authentication against `userRegister`ed users.
-- to be used with `validate`
userValidate :: (FormInput view,MonadIO m) => (UserStr,PasswdStr) -> m (Maybe view)
userValidate (u,p) = liftIO $  do
   Auth _ val <- getAuthMethod
   val u p >>= return .  fmap  fromStr



-- | for compatibility with the same procedure in 'MFLow.Forms.Test.askt'.
-- This is the non testing version
--
-- > askt v w= ask w
--
-- hide one or the other
askt :: FormInput v => (Int -> a) -> View v IO a -> FlowM v IO a
askt v w =  ask w


-- | It is the way to interact with the user.
-- It takes a widget and return the input result. If the widget is not validated (return @Nothing@)
-- , the page is presented again
--
-- If the environment or the URL has the parameters being looked at, maybe as a result of a previous interaction,
-- it will not ask to the user and return the result.
-- To force asking in any case, add an `clearEnv` statement before.
-- It also handles ajax requests
--
-- 'ask' also synchronizes the execution of the flow with the user page navigation by

-- * Backtracking (invoking previous 'ask' staement in the flow) when detecting mismatches between
-- get and post parameters and what is expected by the widgets
-- until a total or partial match is found.
--
-- * Advancing in the flow by matching a single requests with one or more sucessive ask statements
--
-- Backtracking and advancing can occur in a single request, so the flow in any state can reach any
-- other state in the flow if the request has the required parameters.
ask :: (FormInput view) =>
       View view IO a -> FlowM view IO a
ask w =  do
 resetCachePolicy
 st1 <- get >>= \s -> return s{mfSequence=
                                   let seq= mfSequence s in
                                   if seq ==inRecovery then 0 else seq
                              ,mfHttpHeaders =[],mfAutorefresh= False }
 if not . null $ mfTrace st1 then fail "" else do
  -- AJAX
  let env= mfEnv st1
      mv1= lookup "ajax" env
      majax1= mfAjax st1

  case (majax1,mv1,M.lookup (fromJust mv1)(fromJust majax1), lookup "val" env)  of
   (Just ajaxl,Just v1,Just f, Just v2) -> do
     FlowM . lift $ (unsafeCoerce f) v2
     FlowM $ lift nextMessage
     ask w
  -- END AJAX

   _ ->   do

--  mfPagePath : contains the REST path of the page.
--  it is set for each page
--  if it does not exist then it comes from a state recovery, backtrack (to fill-in the field)
    let pagepath = mfPagePath st1
    if null pagepath then fail ""                                   -- !> "null pagepath"
--  if exist and it is not prefix of the current path being navigated to, backtrack
      else if not $  pagepath `isPrefixOf` mfPath st1 then fail ""    -- !> ("pagepath fail with "++ show (mfPath st1))
       else do

     let st= st1{needForm= NoElems, inSync= False, linkMatched= False
                 ,mfRequirements= []
                 ,mfInstalledScripts=  if newAsk st1 then [] else mfInstalledScripts st1}
     put st
     FormElm forms mx <- FlowM . lift  $ runView  w            --  !> "eval"
     setCachePolicy
     st' <- get
     if notSyncInAction st' then put st'{notSyncInAction=False}>> ask w

      else
      case mx  of
       Just x -> do
         put st'{newAsk= True, mfEnv=[]}
         breturn x                                    -- !> ("BRETURN "++ show (mfPagePath st') )

       Nothing ->
         if  not (inSync st')  && not (newAsk st')
                                                      --  !> ("insync="++show (inSync st'))
                                                      --  !> ("newask="++show (newAsk st'))
          then fail ""                                --  !> "FAIL sync"
          else if mfAutorefresh st' then do
                     resetState st st'                --  !> ("EN AUTOREFRESH" ++ show [ mfPagePath st,mfPath st,mfPagePath st'])
--                     modify $ \st -> st{mfPagePath=mfPagePath st'} !> "REPEAT"
                     FlowM $ lift  nextMessage
                     ask w
          else do
             reqs <-  FlowM $ lift installAllRequirements    --   !> "REPEAT"
             st' <- get                                      --   !> (B.unpack $ toByteString reqs)
             let header= mfHeader st'
                 t= mfToken st'
             cont <- case (needForm st') of
                      HasElems  ->  do
                               frm <- formPrefix  st' forms False   -- !> ("formPrefix="++ show(mfPagePath st'))
                               return . header $ reqs <> frm
                      _     -> return . header $ reqs <> forms

             let HttpData ctype c s= toHttpData cont
             liftIO . sendFlush t $ HttpData (ctype ++ mfHttpHeaders st') (mfCookies st' ++ c) s

             resetState st st'
             FlowM $ lift  nextMessage         -- !> "NEXTMESSAGE"
             ask w
    where
    resetState st st'=
             put st{mfCookies=[]
                   -- if autorefresh, keep the list of installed scripts
                   ,mfInstalledScripts= if mfAutorefresh st' then mfInstalledScripts st' else []
                   ,newAsk= False
                   ,mfToken= mfToken st'
                   ,mfPageFlow= mfPageFlow st'
                   ,mfAjax= mfAjax st'
                   ,mfData= mfData st'
                   ,mfSomeNotValidates= False}


-- | A synonym of ask.
--
-- Maybe more appropiate for pages with long interactions with the user
-- while the result has little importance.
page
  :: (FormInput view) =>
      View view IO a -> FlowM view IO a
page= ask

nextMessage :: MonadIO m =>  WState view m ()
nextMessage = do
     st <- get
     let t= mfToken st
         t1= mfkillTime st
         t2= mfSessionTime st
     msg <- liftIO ( receiveReqTimeout t1 t2  t)
     let req=   getParams msg
         env=   updateParams inPageFlow (mfEnv st) req   -- !> ("PAGEFLOW="++ show inPageFlow)
         npath= pwfPath msg
         path=  mfPath st
         inPageFlow= mfPagePath st `isPrefixOf` npath

     put st{ mfPath= npath
           , mfPageFlow= inPageFlow
           , mfEnv= env }

     where

--     comparePaths _ n [] xs=  n
--     comparePaths  o n _ [] = o
--     comparePaths  o n (v:path) (v': npath) | v== v' = comparePaths o (n+1)path npath
--                                        | otherwise= n

     updateParams :: Bool -> Params -> Params -> Params
     updateParams False _ req= req
     updateParams True env req=
        let old= takeWhile isparam  env
            (new,rest)= Data.List.break isparam  req
            parms= new++ old++ rest
        
--        let params= takeWhile isparam  env
--            fs= fst $ head req
--            parms= (case findIndex (\p -> fst p == fs)  params of
--                      Nothing -> params
--                      Just  i -> Data.List.take i params)
--                    ++  req
        in parms
--                 !> "IN PAGE FLOW"  !>  ("parms=" ++ show parms )
--                                    !>  ("env=" ++ show env)
--                                    !>  ("req=" ++ show req)



isparam ('p': r:_,_)=   isNumber r
isparam ('c': r:_,_)=   isNumber r
isparam _= False

-- | Creates a stateless flow (see `stateless`) whose behaviour is defined as a widget. It is a
-- higuer level form of the latter
wstateless
  :: (Typeable view,  FormInput view) =>
     View view IO () -> Flow
wstateless w =  runFlow . transientNav . ask $ w **> (stop `asTypeOf` w)






-- | Wrap a widget with form element within a form-action element.
-- Usually this is not necessary since this wrapping is done automatically by the @Wiew@ monad, unless
-- there are more than one form in the page.
wform ::  (Monad m, FormInput view)
          => View view m b -> View view m b
wform= insertForm
--wform x = View $ do
--     FormElm form mr <- (runView $   x )
--     st <- get
--     form1 <- formPrefix st form True
--     put st{needForm=HasForm}
--     return $ FormElm form1 mr




resetButton :: (FormInput view, Monad m) => String -> View view m ()
resetButton label= View $ return $ FormElm (finput  "reset" "reset" label False Nothing)
                        $ Just ()

submitButton :: (FormInput view, Monad m) => String -> View view m String
submitButton label= getParam Nothing "submit" $ Just label

newtype AjaxSessionId= AjaxSessionId String deriving Typeable

-- | Install the server code and return the client code for an AJAX interaction.
-- It is very lightweight, It does no t need jQuery.
--
-- This example increases the value of a text box each time the box is clicked
--
-- >  ask $ do
-- >        let elemval= "document.getElementById('text1').value"
-- >        ajaxc <- ajax $ \n -> return $ elemval <> "='" <> B.pack(show(read  n +1)) <> "'"
-- >        b <<  text "click the box"
-- >          ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc elemval)]
ajax :: (MonadIO m, FormInput v)
     => (String ->  View v m ByteString)  -- ^ user defined procedure, executed in the server.Receives the value of the javascript expression and must return another javascript expression that will be executed in the web browser
     ->  View v m (String -> String)      -- ^ returns a function that accept a javascript expression and return a javascript event handler expression that invokes the ajax server procedure
ajax  f =  do
     requires[JScript ajaxScript]
     t <- gets mfToken
     id <- genNewId
     installServerControl id $ \x-> do
          setSessionData $ AjaxSessionId id
          r <- f x
          liftIO $ sendFlush t  (HttpData [("Content-Type", "text/plain")][] r )
          return ()

installServerControl :: (FormInput v,MonadIO m) => String -> (String -> View v m ()) -> View v m (String -> String)
installServerControl id f= do
      t <- gets mfToken
      st <- get
      let ajxl = fromMaybe M.empty $ mfAjax st
      let ajxl'= M.insert id (unsafeCoerce f ) ajxl
      put st{mfAjax=Just ajxl'}
      return $ \param ->  "doServer("++"'" ++  twfname t ++"','"++id++"',"++ param++")"

-- | Send the javascript expression, generated by the procedure parameter as a ByteString, execute it in the browser and the result is returned back
--
-- The @ajaxSend@ invocation must be inside a ajax procedure or else a /No ajax session set/ error will be produced
ajaxSend
  :: (Read a,Monoid v, MonadIO m) => View v m ByteString -> View v m a
ajaxSend cmd=  View $ do
   AjaxSessionId id <- getSessionData `onNothing` error "no AjaxSessionId set"
   env <- getEnv
   t <- getToken
   case (lookup "ajax" $ env, lookup "val" env) of
       (Nothing,_) -> return $ FormElm mempty Nothing
       (Just id, Just _) -> do
           FormElm __ (Just  str) <- runView  cmd
           liftIO $ sendFlush t  $ HttpData [("Content-Type", "text/plain")][] $ str <>  readEvalLoop t id "''"
           nextMessage
           env <- getEnv
           case (lookup "ajax" $ env,lookup "val" env) of
               (Nothing,_) -> return $ FormElm mempty Nothing
               (Just id, Just v2) -> do
                    return $ FormElm mempty  . Just  $ read v2
   where
   readEvalLoop t id v = "doServer('"<> fromString (twfname t)<>"','"<> fromString id<>"',"<>v<>");" :: ByteString

-- | Like @ajaxSend@ but the result is ignored
ajaxSend_
  :: (MonadIO m, Monoid v) => View v m ByteString -> View v m ()
ajaxSend_ = ajaxSend

wlabel
  :: (Monad m, FormInput view) => view -> View view m a -> View view m a
wlabel str w = do
   id <- genNewId
   ftag "label" str `attrs` [("for",id)] ++> w <! [("id",id)]


-- | Creates a link to a the next step within the flow.
-- A link can be composed with other widget elements.
--  It can not be broken by its own definition.
-- It points to the page that created it.
wlink :: (Typeable a, Show a, MonadIO m,  FormInput view)
         => a -> view -> View  view m a
wlink x v=    View $ do
      verb <- getWFName
      st   <- get

      let name = --mfPrefix st ++
                (map toLower $ if typeOf x== typeOf(undefined :: String)
                                   then unsafeCoerce x
                                   else show x)
          lpath = mfPath st
          newPath= mfPagePath st  ++ [name]

      r <- if  linkMatched st  then return Nothing -- only a link match per page or monadic sentence in page
          else
             case  newPath `isPrefixOf` lpath   of
             True -> do
                  modify $ \s -> s{inSync= True
                                 ,linkMatched= True
                                 ,mfPagePath= newPath }

                  return $ Just x
--                         !> (name ++ "<-" ++ "link path=" ++show newPath)
             False ->  return Nothing
--                         !> ( "NOT MATCHED "++name++" link path= "++show newPath
--                             ++ "path="++  show lpath)

      let path= concat ['/':v| v <- newPath ]
      return $ FormElm (flink path v) r

-- Creates an absolute link. While a `wlink` path depend on the page where it is located and
-- ever points to the code of the page that had it inserted, an absLink point to the first page
-- in the flow that inserted it. It is useful for creating a backtracking point in combination with `retry`
--
-- >   page $ absLink "here" << p << "here link"
-- >   page $ p << "second page" ++> wlink () << p << "click here"
-- >   page $ p << "third page" ++> retry (absLink "here" << p << "will go back")
-- >   page $ p << "fourth page" ++> wlink () << p << "will not reach here"
--
-- After navigating to the third page, when
-- ckicking in the link, will backtrack to the first, and will validate the first link as if the click
-- where done in the first page. Then the second page would be displayed.
--
-- In monadic widgets, it also backtrack to the statement where the absLink is located without the
-- need of retry:
--
-- >   page $ do
-- >     absLink "here" << p << "here link"
-- >     p << "second statement" ++> wlink () << p << "click here"
-- >     p << "third statement" ++> (absLink "here" << p << "will present the first statement alone")
-- >     p << "fourth statement" ++> wlink () << p << "will not reach here"
--absLink x  = wcached  (show x) 0 . wlink x
absLink x v=    View $ do
      verb <- getWFName
      st   <- get

      let name = -- mfPrefix st
                (map toLower $ if typeOf x== typeOf(undefined :: String)
                                   then unsafeCoerce x
                                   else show x)

          lpath = mfPath st
          newPath= mfPagePath st ++ [name]
      r <- if  linkMatched st  then return Nothing -- only a link match per page or monadic sentence in page
           else
             case  newPath `isPrefixOf` lpath   of
             True -> do
                  modify $ \s -> s{inSync= True
                                 ,linkMatched= True
                                 ,mfPagePath= newPath }

                  return $ Just x                             --  !> (name ++ "<- abs" ++ "lpath=" ++show lpath)
             False ->  return Nothing                         --  !> ( "NOT MATCHED "++name++" LP= "++show  lpath)

      path <- liftIO $ cachedByKey (show x) 0 . return $ currentPath st ++ ('/':name)

      return $ FormElm (flink path v) r  -- !> name



-- | When some user interface return some response to the server, but it is not produced by
-- a form or a link, but for example by an script, @returning@  convert this code into a
-- widget.
--
-- At runtime the parameter is read from the environment and validated.
--
-- . The parameter is the visualization code, that accept a serialization function that generate
-- the server invocation string, used by the visualization to return the value by means
-- of an script, usually.
returning :: (Typeable a, Read a, Show a,Monad m, FormInput view)
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
      r <- getParam1 name env
      return $ FormElm toSend $ valToMaybe r





--instance (Widget a b m view, Monoid view) => Widget [a] b m view where
--  widget xs = View $ do
--      forms <- mapM(\x -> (runView  $  widget x )) xs
--      let vs  = concatMap (\(FormElm v _) -> v) forms
--          res = filter isJust $ map (\(FormElm _ r) -> r) forms
--          res1= if null res then Nothing else head res
--      return $ FormElm [mconcat vs] res1

-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (FormInput view, Monad m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= foldl' (<|>) noWidget xs
--  View $ do
--      forms <- mapM runView  xs
--      let vs  = concatMap (\(FormElm v _) ->  [mconcat v]) forms
--          res = filter isJust $ map (\(FormElm _ r) -> r) forms
--          res1= if null res then Nothing else head res
--      return $ FormElm  vs res1

-- | from a list of widgets, it return the validated ones.
manyOf :: (FormInput view, MonadIO m, Functor m)=> [View view m a]  -> View view m [a]
manyOf xs= whidden () *> (View $ do
      forms <- mapM runView  xs
      let vs  = mconcat $ map (\(FormElm v _) ->   v) forms
          res1= catMaybes $ map (\(FormElm _ r) -> r) forms
      nval <- gets mfSomeNotValidates
      return . FormElm vs $ if nval then Nothing else Just res1)

-- | like manyOf, but does not validate if one or more of the widgets does not validate
allOf xs= manyOf xs `validate` \rs ->
      if length rs== length xs
         then return Nothing
         else return $ Just "Not all of the required data completed"

(>:>) :: (Monad m, Monoid v) => View v m a -> View v m [a]  -> View v m [a]
(>:>) w ws = View $ do
    FormElm fs mxs <- runView $  ws
    FormElm f1 mx  <- runView w
    return $ FormElm (f1 <> fs)
         $ case( mx,mxs) of
             (Just x, Just xs) -> Just $ x:xs
             (Nothing, mxs)    -> mxs
             (Just x, _)       -> Just [x]

-- | Intersperse a widget in a list of widgets. the results is a 2-tuple of both types.
--
-- it has a infix priority @infixr 5@
(|*>) :: (MonadIO m, Functor m, FormInput view)
           => View view m r
           -> [View view m r']
           -> View view m (Maybe r,Maybe r')
(|*>) x xs= View $ do
  fs <-  mapM runView  xs
  FormElm fx rx   <- runView  x
  let (fxs, rxss) = unzip $ map (\(FormElm v r) -> (v,r)) fs
      rs= filter isJust rxss
      rxs= if null rs then Nothing else  head rs
  return $ FormElm (fx <> mconcat (intersperse  fx fxs) <> fx)
         $ case (rx,rxs) of
            (Nothing, Nothing) -> Nothing
            other -> Just other



infixr 5 |*>

-- | Put a widget before and after other. Useful for navigation links in a page that appears at toAdd
-- and at the bottom of a page.

-- It has a low infix priority: @infixr 1@
(|+|) :: (Functor m, FormInput view, MonadIO m)
      => View view m r
      -> View view m r'
      -> View view m (Maybe r, Maybe r')
(|+|) w w'=  w |*> [w']

infixr 1 |+|


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

--infixr 7 .<<.
---- | > (.<<.) w x = w $ toByteString x
--(.<<.) :: (FormInput view) => (ByteString -> ByteString) -> view -> ByteString
--(.<<.) w x = w ( toByteString x)
--
---- | > (.<+>.) x y = normalize x <+> normalize y
--(.<+>.)
--  :: (Monad m, FormInput v, FormInput v1) =>
--     View v m a -> View v1 m b -> View ByteString m (Maybe a, Maybe b)
--(.<+>.) x y = normalize x <+> normalize y
--
---- | > (.|*>.) x y = normalize x |*> map normalize y
--(.|*>.)
--  :: (Functor m, MonadIO m, FormInput v, FormInput v1) =>
--     View v m r
--     -> [View v1 m r'] -> View ByteString m (Maybe r, Maybe r')
--(.|*>.) x y = normalize x |*> map normalize y
--
---- | > (.|+|.) x y = normalize x |+| normalize y
--(.|+|.)
--  :: (Functor m, MonadIO m, FormInput v, FormInput v1) =>
--     View v m r -> View v1 m r' -> View ByteString m (Maybe r, Maybe r')
--(.|+|.) x y = normalize x |+| normalize y
--
---- | > (.**>.) x y = normalize x **> normalize y
--(.**>.)
--  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
--     View v m a -> View v1 m b -> View ByteString m b
--(.**>.) x y = normalize x **> normalize y
--
---- | > (.<**.) x y = normalize x <** normalize y
--(.<**.)
--  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
--     View v m a -> View v1 m b -> View ByteString m a
--(.<**.) x y = normalize x <** normalize y
--
---- | > (.<|>.) x y= normalize x <|> normalize y
--(.<|>.)
--  :: (Monad m, Functor m, FormInput v, FormInput v1) =>
--     View v m a -> View v1 m a -> View ByteString m a
--(.<|>.) x y= normalize x <|> normalize y
--
---- | > (.<++.) x v= normalize x <++ toByteString v
--(.<++.) :: (Monad m, FormInput v, FormInput v') => View v m a -> v' -> View ByteString m a
--(.<++.) x v= normalize x <++ toByteString v
--
---- | > (.++>.) v x= toByteString v ++> normalize x
--(.++>.) :: (Monad m, FormInput v, FormInput v') => v -> View v' m a -> View ByteString m a
--(.++>.) v x= toByteString v ++> normalize x


instance FormInput  ByteString  where
    toByteString= id
    toHttpData = HttpData [contentHtml ] []
    ftag x= btag x []
    inred = btag "b" [("style", "color:red")]
    finput n t v f c= btag "input"  ([("type", t) ,("name", n),("value",  v)] ++ if f then [("checked","true")]  else []
                              ++ case c of Just s ->[( "onclick", s)]; _ -> [] ) ""
    ftextarea name text= btag "textarea"  [("name", name)]   $ fromChunks [encodeUtf8 text]

    fselect name   options=  btag "select" [("name", name)]   options

    foption value content msel= btag "option" ([("value",  value)] ++ selected msel)   content
            where
            selected msel = if  msel then [("selected","true")] else []

    attrs = addAttrs


    formAction action method form = btag "form" [("action", action),("method", method)]  form
    fromStr = fromString
    fromStrNoEncode= fromString

    flink  v str = btag "a" [("href",  v)]  str

------ page Flows ----

-- | Prepares the state for a page flow. It add a prefix to every form element or link identifier for the formlets and also
-- keep the state of the links clicked and form imput entered within the widget.
-- If the computation within the widget has branches  @if@ @case@ etc, each branch must have its pageFlow with a distinct identifier.
-- See <http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html>
pageFlow
  :: (Monad m, Functor m, FormInput view) =>
     String -> View view m a -> View view m a
pageFlow str widget=do
     s <- get

     if   mfPageFlow s == False
       then do
       put s{mfPrefix= str ++ mfPrefix s
            ,mfSequence=0
            ,mfPageFlow= True
             }                               -- !> ("PARENT pageflow. prefix="++ str)

       r<- widget <** (modify (\s' -> s'{mfSequence= mfSequence s
                                   ,mfPrefix= mfPrefix s
                                   }))
       modify (\s -> s{mfPageFlow=False} )
       return r                                                                                 -- !> ("END PARENT pageflow. prefix="++ str))


       else do
       put s{mfPrefix= str++ mfPrefix s,mfSequence=0}      -- !> ("PARENT pageflow. prefix="++ str)                                                                 --  !> ("CHILD pageflow. prefix="++ str)

       widget <** (modify (\s' -> s'{mfSequence= mfSequence s
                                 ,mfPrefix= mfPrefix s}))
                                                                                                   -- !> ("END CHILD pageflow. prefix="++ str))


                                                                                 -- !> ("END CHILD pageflow. prefix="++ str))
-- | send raw data to the client.
--
-- example
--
-- >rawSend $ HttpData  [("Content-Type","text/plain"), ("Cache-Control", "max-age=360000")] [] "hello"
rawSend :: (FormInput v,MonadIO m) => HttpData -> View v m ()
rawSend dat=  do
    tok <- getToken
    liftIO $ sendFlush tok dat
    modify $ \st -> st{mfAutorefresh= True}
