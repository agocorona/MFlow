{- |
Some dynamic widgets, widgets that dynamically edit content in other widgets,
widgets for templating, content management and multilanguage. And some primitives
to create other active widgets.
-}
-- {-# OPTIONS -F -pgmF cpphs  #-}
{-# OPTIONS -cpp  -pgmPcpphs  -optP--cpp #-}
{-# LANGUAGE  UndecidableInstances,ExistentialQuantification
            , FlexibleInstances, OverlappingInstances, FlexibleContexts
            , OverloadedStrings, DeriveDataTypeable , ScopedTypeVariables
            , StandaloneDeriving #-}




module MFlow.Forms.Widgets (
-- * Ajax refreshing of widgets
autoRefresh, noAutoRefresh, appendUpdate, prependUpdate, push, UpdateMethod(..), lazy

-- * JQueryUi widgets
,datePicker, getSpinner, wautocomplete, wdialog,

-- * User Management
userFormOrName,maybeLogout, wlogin,

-- * Active widgets
wEditList,wautocompleteList
, wautocompleteEdit,

-- * Editing widgets
delEdited, getEdited, setEdited, prependWidget,appendWidget,setWidget

-- * Content Management
,tField, tFieldEd, htmlEdit, edTemplate, dField, template, witerate,tfieldKey

-- * Multilanguage
,mFieldEd, mField

-- * utility
,insertForm, readtField, writetField


) where
import MFlow
import MFlow.Forms
import MFlow.Forms.Internals
import Data.Monoid
import Data.ByteString.Lazy.UTF8 as B hiding (length,span)
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Monad.Trans
import Data.Typeable
import Data.List
import System.IO.Unsafe

import Control.Monad.State
import Data.TCache
import Data.TCache.Defs
import Data.TCache.Memoization
import Data.RefSerialize hiding ((<|>))
import qualified Data.Map as M
import Data.IORef
import MFlow.Cookies
import Data.Maybe
import Data.Char
import Control.Monad.Identity
import Control.Workflow(killWF)
import Unsafe.Coerce
import Control.Exception
import MFlow.Forms.Cache




--jqueryScript= "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
--jqueryScript1="//code.jquery.com/jquery-1.9.1.js"
--
--jqueryCSS1= "//code.jquery.com/ui/1.9.1/themes/base/jquery-ui.css"
--jqueryCSS= "//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"
--
--jqueryUI1= "//code.jquery.com/ui/1.9.1/jquery-ui.js"
--jqueryUI= "//code.jquery.com/ui/1.10.3/jquery-ui.js"

jqueryScript= getConfig "cjqueryScript" "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
jqueryCSS=    getConfig "cjqueryCSS"    "//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"
jqueryUI=     getConfig "cjqueryUI"     "//code.jquery.com/ui/1.10.3/jquery-ui.js"
nicEditUrl=   getConfig "cnicEditUrl"   "//js.nicedit.com/nicEdit-latest.js"
------- User Management ------

-- | Present a user form if not logged in. Otherwise, the user name and a logout link is presented.
-- The parameters and the behavior are the same as 'userWidget'.
-- Only the display is different
userFormOrName  mode wid= userWidget mode wid `wmodify` f  <** maybeLogout
  where
  f _ justu@(Just u)  =  return ([fromStr u], justu) -- !> "input"
  f felem Nothing = do
     us <- getCurrentUser -- getEnv cookieuser
     if us == anonymous
           then return (felem, Nothing)
           else return([fromStr us],  Just us)

-- | Display a logout link if the user is logged. Nothing otherwise
maybeLogout :: (MonadIO m,Functor m,FormInput v) => View v m ()
maybeLogout= do
    us <- getCurrentUser
    if us/= anonymous
      then do
          cmd <- ajax $ const $ return "window.location=='/'" --refresh
          fromStr " " ++> ((wlink () (fromStr "logout")) <![("onclick",cmd "''")]) `waction` const logout
      else noWidget


data Medit view m a = Medit (M.Map B.ByteString [(String,View view m a)])

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)

  deriving  Typeable

#else

instance (Typeable view, Typeable a) => Typeable (Medit view m a) where
  typeOf= \v -> mkTyConApp (mkTyCon3 "MFlow" "MFlow.Forms.Widgets" "Medit" )
                [typeOf (tview v)
                ,typeOf (ta v)]
      where
      tview :: Medit v m a -> v
      tview= undefined
      tm :: Medit v m a -> m a
      tm= undefined
      ta :: Medit v m a -> a
      ta= undefined

#endif

-- | If not logged, it present a page flow which asks for the user name, then the password if not logged
--
-- If logged, it present the user name and a link to logout
--
-- normally to be used with autoRefresh and pageFlow when used with other widgets.
wlogin :: (MonadIO m,Functor m,FormInput v) => View v m ()
wlogin=  wform $ do
   username <- getCurrentUser
   if username /= anonymous
         then do
           private; noCache;noStore
           return username
         else do
          name <- getString Nothing <! hint "login name"
                                    <! size (9 :: Int)
                  <++ ftag "br" mempty
          pass <- getPassword <! hint "password"
                              <! size 9
                     <++ ftag "br" mempty
                     <** submitButton "login"
          val  <- userValidate (name,pass)
          case val of
            Just msg -> notValid msg
            Nothing  -> login name >> (return name)

   `wcallback` (\name -> ftag "b" (fromStr $ "logged as " ++ name++ " ")
                     ++> pageFlow "logout" (submitButton "logout")) -- wlink ("logout" :: String) (ftag "b" $ fromStr " logout"))
   `wcallback`  const (logout >> wlogin)

focus = [("onload","this.focus()")]
hint s= [("placeholder",s)]
size n= [("size",show n)]

getEdited1 id= do
    Medit stored <- getSessionData `onNothing` return (Medit M.empty)
    return $ fromMaybe [] $ M.lookup id stored

-- | Return the list of edited widgets (added by the active widgets) for a given identifier
getEdited

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)

     :: (Typeable v, Typeable a, Typeable m1, MonadState (MFlowState view) m) =>

#else

    :: (Typeable v, Typeable a, MonadState (MFlowState view) m) =>

#endif

      B.ByteString -> m [View v m1 a]

getEdited id= do
  r <- getEdited1 id
  let (_,ws)= unzip r
  return ws

-- | Deletes the list of edited widgets for a certain identifier and with the type of the witness widget parameter
delEdited
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
    :: (Typeable v, Typeable a, MonadIO m, Typeable m1,
#else
    :: (Typeable v, Typeable a, MonadIO m,
#endif
  MonadState (MFlowState view) m)
       => B.ByteString           -- ^ identifier
         -> [View v m1 a] -> m ()  -- ^ witness
delEdited id witness=do
    Medit stored <-  getSessionData `onNothing` return (Medit (M.empty))
    let (ks, ws)=  unzip $ fromMaybe [] $ M.lookup id stored

    return $ ws `asTypeOf` witness
    liftIO $ mapM  flushCached ks
    let stored'= M.delete id  stored
    setSessionData . Medit $ stored'



--    setEdited id ([] `asTypeOf` (zip (repeat "") witness))

setEdited id ws= do
    Medit stored <-  getSessionData `onNothing` return (Medit (M.empty))
    let stored'= M.insert id ws stored
    setSessionData . Medit $ stored'


addEdited id w= do
    ws <- getEdited1 id
    setEdited id (w:ws)

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
modifyWidget :: (MonadIO m,Executable m,Typeable a,FormInput v, Typeable Identity, Typeable m)
#else
modifyWidget :: (MonadIO m,Executable m,Typeable a,FormInput v)
#endif
    => B.ByteString -> B.ByteString -> View v Identity a -> View v m B.ByteString
modifyWidget selector modifier  w = View $ do
     ws <- getEdited selector
     let n =  length (ws `asTypeOf` [w])
     let key= "widget"++ show selector ++  show n ++ show (typeOf $ typ w)
     let cw =  wcached key 0  w
     addEdited selector (key,cw)
     FormElm form _ <-  runView cw
     let elem=  toByteString   form
     return . FormElm mempty . Just $   selector <> "." <> modifier <> "('" <> elem <> "');"
     where
     typ :: View v m a -> a
     typ = undefined



-- | Return the JavaScript to be executed on the browser to prepend a widget to the location
-- identified by the selector (the bytestring parameter), The selector must have the form of a jQuery expression
-- . It stores the added widgets in the edited list, that is accessed with 'getEdited'
--
-- The resulting string can be executed in the browser. 'ajax' will return the code to
-- execute the complete ajax roundtrip. This code returned by ajax must be in an event handler.
--
-- This example  will insert a widget in the div  when the element with identifier
-- /clickelem/  is clicked. when the form is submitted, the widget values are returned
-- and the list of edited widgets are deleted.
--
-- >    id1<- genNewId
-- >    let sel= "$('#" <>  fromString id1 <> "')"
-- >    callAjax <- ajax . const $ prependWidget sel wn
-- >    let installevents= "$(document).ready(function(){\
-- >              \$('#clickelem').click(function(){"++callAjax "''"++"});})"
-- >
-- >    requires [JScriptFile jqueryScript [installevents] ]
-- >    ws <- getEdited sel
-- >    r <-  (div <<< manyOf ws) <! [("id",id1)]
-- >    delEdited sel ws'
-- >    return  r

prependWidget
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
  :: (Typeable a, MonadIO m, Executable m, FormInput v, Typeable Identity,  Typeable m)
#else
  :: (Typeable a, MonadIO m, Executable m, FormInput v)
#endif
  => B.ByteString           -- ^ jQuery selector
  -> View v Identity a      -- ^ widget to prepend
  -> View v m B.ByteString  -- ^ string returned with the jQuery string to be executed in the browser
prependWidget sel w= modifyWidget sel "prepend" w

-- | Like 'prependWidget' but append the widget instead of prepend.
appendWidget
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
  :: (Typeable a, MonadIO m, Executable m, FormInput v, Typeable Identity,  Typeable m) =>
#else
     :: (Typeable a, MonadIO m, Executable m, FormInput v) =>
#endif
        B.ByteString -> View v Identity a -> View v m B.ByteString
appendWidget sel w= modifyWidget sel "append" w

-- | Like 'prependWidget' but set the entire content of the selector instead of prepending an element
setWidget
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
  :: (Typeable a, MonadIO m, Executable m, FormInput v, Typeable Identity, Typeable m) =>
#else
   :: (Typeable a, MonadIO m, Executable m, FormInput v) =>
#endif
     B.ByteString -> View v Identity a -> View v m B.ByteString
setWidget sel w= modifyWidget sel "html" w


-- | Inside a tag, it add and delete widgets of the same type. When the form is submitted
-- or a wlink is pressed, this widget return the list of validated widgets.
-- the event for adding a new widget is attached , as a click event to the element of the page with the identifier /wEditListAdd/
-- that the user will choose.
--
-- This example add or delete editable text boxes, with two initial boxes   with
-- /hi/, /how are you/ as values. Tt uses blaze-html:
--
-- >  r <-  ask  $   addLink
-- >              ++> br
-- >              ++> (El.div `wEditList`  getString1 $  ["hi", "how are you"]) "addid"
-- >              <++ br
-- >              <** submitButton "send"
-- >
-- >  ask $   p << (show r ++ " returned")
-- >      ++> wlink () (p << text " back to menu")
-- >  mainmenu
-- >  where
-- >  addLink = a ! At.id  "addid"
-- >              ! href "#"
-- >              $ text "add"
-- >  delBox  =  input ! type_   "checkbox"
-- >                   ! checked ""
-- >                   ! onclick "this.parentNode.parentNode.removeChild(this.parentNode)"
-- >  getString1 mx= El.div  <<< delBox ++> getString  mx <++ br

wEditList :: (Typeable a,Read a
             ,FormInput view
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
             ,Functor m,MonadIO m, Executable m, Typeable m, Typeable Identity)
#else
             ,Functor m,MonadIO m, Executable m)
#endif
          => (view ->view)     -- ^ The holder tag
          -> (Maybe a -> View view Identity a) -- ^ the contained widget, initialized  by a value of its type
          -> [a]          -- ^ The initial list of values.
          -> String            -- ^ The id of the button or link that will create a new list element when clicked
          -> View view m  [a]
wEditList holderview w xs addId = pageFlow addId $ do
    let ws=  map (w . Just) xs
        wn=  w Nothing
    id1<- genNewId
    let sel= "$('#" <>  fromString id1 <> "')"
    callAjax <- ajax . const $ prependWidget sel wn
    let installevents= "$(document).ready(function(){$('#"++addId++"').click(function(){"++callAjax "''"++"});})"

    requires [JScriptFile jqueryScript [installevents] ]

    ws' <- getEdited sel
    r <-  (holderview  <<< (manyOf $ ws' ++ map changeMonad ws)) <! [("id",id1)]
    delEdited sel ws'
    return r


-- | Present the JQuery auto-completion list, from a procedure defined by the programmer, to a text box.
wautocomplete
  :: (Show a, MonadIO m, FormInput v)
  => Maybe String       -- ^ Initial value
  -> (String -> IO a)   -- ^ Auto-completion procedure: will receive a prefix and return a list of strings
  -> View v m String
wautocomplete mv autocomplete  = do
    text1 <- genNewId
    ajaxc <- ajax $ \u -> do
                          r <- liftIO $ autocomplete u
                          return $ jaddtoautocomp text1 r


    requires [JScriptFile jqueryScript [] -- [events]
             ,CSSFile jqueryCSS
             ,JScriptFile jqueryUI []]


    getString mv <! [("type", "text")
                    ,("id", text1)
                    ,("oninput", ajaxc $ "$('#"++text1++"').attr('value')" )
                    ,("autocomplete", "off")]


    where
    jaddtoautocomp text1 us= "$('#"<>fromString text1<>"').autocomplete({ source: " <> fromString( show us) <> "  });"


-- | Produces a text box. It gives a auto-completion list to the textbox. When return
-- is pressed in the textbox, the box content is used to create a widget of a kind defined
-- by the user, which will be situated above of the textbox. When submitted, the result is the content
-- of the created widgets (the validated ones).
--
-- 'wautocompleteList' is an specialization of this widget, where
-- the widget parameter is fixed, with a checkbox that delete the element when unselected
-- . This fixed widget is as such (using generic 'FormElem' class tags):
--
-- > ftag "div"    <<< ftag "input" mempty
-- >                               `attrs` [("type","checkbox")
-- >                                       ,("checked","")
-- >                                       ,("onclick","this.parentNode.parentNode.removeChild(this.parentNode)")]
-- >               ++> ftag "span" (fromStr $ fromJust x )
-- >               ++> whidden( fromJust x)
wautocompleteEdit
    :: (Typeable a, MonadIO m,Functor m, Executable m
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
     , FormInput v, Typeable m, Typeable Identity)
#else
     , FormInput v)
#endif
    => String                                 -- ^ the initial text of the box
    -> (String -> IO [String])                -- ^ the auto-completion procedure: receives a prefix, return a list of options.
    -> (Maybe String  -> View v Identity a)          -- ^ the widget to add, initialized with the string entered in the box
    -> [String]                               -- ^ initial set of values
    -> View v m [a]                           -- ^ resulting widget
wautocompleteEdit phold autocomplete  elem values= do
    id1 <- genNewId
    let textx= id1++"text"
    let sel= "$('#" <> fromString id1 <> "')"
    ajaxc <- ajax $ \(c:u) ->
              case c  of
                'f' -> prependWidget sel (elem $ Just u)
                _   -> do
                          r <- liftIO $ autocomplete u
                          return $ jaddtoautocomp textx r


    requires [JScriptFile jqueryScript [events textx ajaxc]
             ,CSSFile jqueryCSS
             ,JScriptFile jqueryUI []]

    ws' <- getEdited sel

    r<- (ftag "div" mempty  `attrs` [("id",  id1)]
      ++> manyOf (ws' ++ (map (changeMonad . elem . Just) values)))
      <++ ftag "input" mempty
             `attrs` [("type", "text")
                     ,("id", textx)
                     ,("placeholder", phold)
                     ,("oninput", ajaxc $ "'n'+$('#"++textx++"').val()" )
                     ,("autocomplete", "off")]
    delEdited sel ws'
    return r
    where
    events textx ajaxc=
         "$(document).ready(function(){   \
           \$('#"++textx++"').keydown(function(){ \
            \if(event.keyCode == 13){  \
                \var v= $('#"++textx++"').val(); \
                \if(event.preventDefault) event.preventDefault();\
                \else if(event.returnValue) event.returnValue = false;" ++
                 ajaxc "'f'+v"++";"++
             "   $('#"++textx++"').val('');\
           \}\
          \});\
         \});"

    jaddtoautocomp textx us= "$('#"<>fromString textx<>"').autocomplete({ source: " <> fromString( show us) <> "  });"


#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
deriving instance Typeable Identity
#endif

-- | A specialization of 'wutocompleteEdit' which make appear each chosen option with
-- a checkbox that deletes the element when unchecked. The result, when submitted, is the list of selected elements.
wautocompleteList
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
  :: (Functor m, MonadIO m, Executable m, FormInput v, Typeable m, Typeable Identity) =>
#else
  :: (Functor m, MonadIO m, Executable m, FormInput v) =>
#endif
     String -> (String -> IO [String]) -> [String] -> View v m [String]
wautocompleteList phold serverproc values=
 wautocompleteEdit phold serverproc  wrender1 values
 where
 wrender1 x= ftag "div" <<< ftag "input" mempty
                                `attrs` [("type","checkbox")
                                        ,("checked","")
                                        ,("onclick","this.parentNode.parentNode.removeChild(this.parentNode)")]
                        ++> ftag "span" (fromStr $ fromJust x )
                        ++> whidden( fromJust x)

------- Templating and localization ---------

data TField  = TField {tfieldKey :: Key, tfieldContent :: B.ByteString}  deriving (Read, Show,Typeable)

instance Indexable TField where
    key (TField k _)= k
    defPath _= "texts/"


instance Serializable TField where
    serialize (TField k content)  = content
    deserialKey k content= TField k content -- applyDeserializers [des1,des2] k bs


    setPersist =   \_ -> Just filePersist



writetField k s= atomically $ writeDBRef (getDBRef k) $ TField k $ toByteString s


readtField text k= atomically $ do
   let ref = getDBRef k
   mr <- readDBRef ref
   case mr of
    Just (TField k v) -> if v /= mempty then return $ fromStrNoEncode $ toString v else return text
    Nothing -> return text

-- | Creates a rich text editor around a text field or a text area widget.
--   This code:
--
-- > page $ p "Insert the text"
-- >    ++> htmlEdit ["bold","italic"] ""
-- >           (getMultilineText "" <! [("rows","3"),("cols","80")]) <++ br
-- >    <** submitButton "enter"
--
--   Creates a rich text area with bold and italic buttons. The buttons are the ones added
--   in the nicEdit editor.
htmlEdit :: (Monad m, FormInput v) =>  [String] -> UserStr -> View v m a -> View v m a
htmlEdit buttons jsuser w = do
  id <- genNewId

  let installHtmlField=
          "\nfunction installHtmlField(muser,cookieuser,name,buttons){\
            \if(muser== '' || document.cookie.search(cookieuser+'='+muser) != -1)\
                  \bkLib.onDomLoaded(function() {\
                    \var myNicEditor = new nicEditor({buttonList : buttons});\
                    \myNicEditor.panelInstance(name);\
                 \})};\n"
      install= "installHtmlField('"++jsuser++"','"++cookieuser++"','"++id++"',"++show buttons++");\n"

  requires [JScript installHtmlField ,JScriptFile nicEditUrl [install]]
  w <! [("id",id)]




-- | A widget that display the content of an  html, But if the user has edition privileges,
-- it permits to edit it in place. So the editor could see the final appearance
-- of what he writes.
--
-- When the user  click the save, the content is saved and
-- identified by the key. Then, from now on, all the users will see the saved
-- content instead of the code content.
--
-- The content is saved in a file by default (/texts/ in this versions), but there is
-- a configurable version (`tFieldGen`). The content of the element and the formatting
-- is cached in memory, so the display is, theoretically, very fast.
--

tFieldEd
  :: (Functor m,  MonadIO m, Executable m,
      FormInput v) =>
      UserStr -> Key -> v -> View v m ()
tFieldEd  muser k  text= wfreeze k 0 $  do
   content <- liftIO $ readtField text k
   nam     <- genNewId
   let ipanel= nam++"panel"
       name= nam++"-"++k
       install= "installEditField('"++muser++"','"++cookieuser++"','"++name++"','"++ipanel++"');\n"
       getTexts :: (Token -> IO ())
       getTexts token = do
         let (k,s):_ = tenv token
         liftIO $ do
           writetField k  $ (fromStrNoEncode s `asTypeOf` text)
           flushCached k
           sendFlush token $ HttpData [] [] ""
           return()

   requires [JScriptFile nicEditUrl [install]
            ,JScript     ajaxSendText
            ,JScript     installEditField
            ,ServerProc  ("_texts",  transient getTexts)]

   us <- getCurrentUser
   when(us== muser) noCache

   (ftag "div" mempty `attrs` [("id",ipanel)]) ++>
    notValid (ftag "span" content `attrs` [("id", name)])



installEditField=
          "\nfunction installEditField(muser,cookieuser,name,ipanel){\
            \if(muser== '' || document.cookie.search(cookieuser+'='+muser) != -1){\
                    \var myNicEditor = new nicEditor({fullPanel : true, onSave : function(content, id, instance) {\
                         \ajaxSendText(id,content);\
                         \myNicEditor.removeInstance(name);\
                         \myNicEditor.removePanel(ipanel);\
                       \}});\
                    \myNicEditor.addInstance(name);\
                    \myNicEditor.setPanel(ipanel);\
                 \}};\n"

ajaxSendText = "\nfunction ajaxSendText(id,content){\
        \var arr= id.split('-');\
        \var k= arr[1];\
        \$.ajax({\
               \type: 'POST',\
               \url: '/_texts',\
               \data: k + '='+ encodeURIComponent(content),\
               \success: function (resp) {},\
               \error: function (xhr, status, error) {\
                        \var msg = $('<div>' + xhr + '</div>');\
                        \id1.html(msg);\
               \}\
           \});\
        \return false;\
        \};\n"

-- | a text field. Read the cached  field value and present it without edition.
tField :: (MonadIO m,Functor m, Executable m, FormInput v)
       => Key
       -> View v m ()
tField k = wfreeze k 0 $ do
    content <- liftIO $ readtField (fromStrNoEncode "not found")  k
    notValid content

-- | A multilanguage version of tFieldEd. For a field with @key@ it add a suffix with the
-- two characters of the language used.
mFieldEd  muser k content= do
  lang <- getLang
  tFieldEd  muser (k ++ ('-':lang)) content



-- | A multilanguage version of tField
mField k= do
  lang <- getLang
  tField $ k ++ ('-':lang)

data IteratedId = IteratedId  String String deriving (Typeable, Show)

-- | Permits to iterate the presentation of data and//or input fields and widgets within
-- a web page that does not change. The placeholders are created with dField.  Both are widget
-- modifiers: The latter gets a widget and create a placeholder in the page that is updated
-- via ajax. The content of the update is the rendering of the widget at each iteration.
-- The former gets a wider widget which contains dField elements and permit the iteration.
-- Whenever a link or a form within the witerate widget is activated, the result is the
-- placeholders filled with the new  html content.  This content can be data, a input field,
-- a link or a widget. No navigation happens.
--
-- This permits even faster updates than autoRefresh.  since the latter refresh the whole
-- widget and it does not permits modifications of the layout at runtime.
--
-- When edTemplate or template is used on top of witerate, the result is editable at runtime,
-- and the span placeholders generated, that are updated via ajax can be relocated within
-- the layout of the template.
--
-- Additionally, contrary to some javascript frameworks, the pages generated with this
-- mechanism are searchable by web crawlers.

witerate
  :: (MonadIO m, Functor m, FormInput v) =>
      View v m a -> View v m a
witerate w= do
   name <- genNewId
   setSessionData $ IteratedId name mempty
   st <- get
   let t= mfkillTime st
   let installAutoEval=
        "$(document).ready(function(){\
           \autoEvalLink('"++name++"',0);\
           \autoEvalForm('"++name++"');\
           \})\n"
   let r = lookup ("auto"++name) $ mfEnv st
       w'= w `wcallback` (const $ do
                              setSessionData $ IteratedId name mempty
                              modify $ \s -> s{mfPagePath=mfPagePath st
                                             ,mfSequence= mfSequence st
                                             ,mfHttpHeaders=[]}
                              w)

   ret <- case r of
    Nothing -> do
     requires [JScript     autoEvalLink
              ,JScript     autoEvalForm
              ,JScript     $ timeoutscript t
              ,JScriptFile jqueryScript [installAutoEval]
              ,JScript     setId]    

     (ftag "div" <<< w') <! [("id",name)] 

    Just sind -> refresh $ View $ do
              FormElm _ mr <- runView w'
              IteratedId _ render <- getSessionData `onNothing` return (IteratedId name mempty)
              return $ FormElm (fromStrNoEncode render) mr

--     View $ do
--         let t= mfToken st
--         modify $ \s -> s{mfRequirements=[],mfHttpHeaders=[]} -- !> "just"
--         resetCachePolicy
--         FormElm _ mr <- runView w'
--         setCachePolicy
--
--         reqs <- installAllRequirements
--
--         st' <- get
--         liftIO . sendFlush t $ HttpData
--                                (mfHttpHeaders st')
--                                (mfCookies st')  (toByteString reqs)
--         put st'{mfAutorefresh=True, inSync=True}
--         return $ FormElm mempty Nothing

   delSessionData $ IteratedId name mempty
   return ret



autoEvalLink = "\nfunction autoEvalLink(id,ind){\
    \var id1= $('#'+id);\
    \var ida= $('#'+id+' a[class!=\"_noAutoRefresh\"]');\
    \ida.off('click');\
    \ida.click(function () {\
     \if (hadtimeout == true) return true;\
     \var pdata = $(this).attr('data-value');\
     \var actionurl = $(this).attr('href');\
     \var dialogOpts = {\
           \type: 'GET',\
           \url: actionurl+'?auto'+id+'='+ind,\
           \data: pdata,\
           \success: function (resp) {\
               \eval(resp);\
               \autoEvalLink(id,ind);\
               \autoEvalForm(id);\
           \},\
           \error: function (xhr, status, error) {\
               \var msg = $('<div>' + xhr + '</div>');\
               \id1.html(msg);\
           \}\
       \};\
     \$.ajax(dialogOpts);\
     \return false;\
    \});\
  \}\n"

autoEvalForm = "\nfunction autoEvalForm(id) {\
    \var buttons= $('#'+id+' input[type=\"submit\"]');\
    \var idform= $('#'+id+' form[class!=\"_noAutoRefresh\"]');\
    \buttons.off('click');\
    \buttons.click(function(event) {\
      \if ($(this).attr('class') != '_noAutoRefresh'){\
        \event.preventDefault();\
        \if (hadtimeout == true) return true;\
        \var $form = $(this).closest('form');\
        \var url = $form.attr('action');\
        \pdata = 'auto'+id+'=true&'+this.name+'='+this.value+'&'+$form.serialize();\
        \postForm(id,url,pdata);\
        \return false;\
        \}else {\
          \noajax= true;\
          \return true;\
        \}\
     \})\
    \\n\
    \var noajax;\
    \idform.submit(function(event) {\
     \if(noajax) {noajax=false; return true;}\
       \event.preventDefault();\
       \var $form = $(this);\
       \var url = $form.attr('action');\
       \var pdata = 'auto'+id+'=true&' + $form.serialize();\
       \postForm(id,url,pdata);\
       \return false;})\
    \}\
    \function postForm(id,url,pdata){\
        \var id1= $('#'+id);\
         \$.ajax({\
            \type: 'POST',\
            \url: url,\
            \data: 'auto'+id+'=true&'+this.name+'='+this.value+'&'+pdata,\
            \success: function (resp) {\
                \eval(resp);\
                \autoEvalLink(id,0);\
                \autoEvalForm(id);\
            \},\
            \error: function (xhr, status, error) {\
                \var msg = $('<div>' + xhr + '</div>');\
                \id1.html(msg);\
            \}\
        \});\
       \}"



setId= "function setId(id,v){document.getElementById(id).innerHTML= v;};\n"

-- | Present a widget via AJAX if it is within a 'witerate' context. In the first iteration it present the
-- widget surrounded by a placeholder. subsequent iterations will send just the javascript code
-- necessary for the refreshing of the placeholder.
dField
  :: (Monad m, FormInput view) =>
     View view m  b -> View view m b
dField w= View $ do
    id <- genNewId
    FormElm render mx <- runView w
    st <- get
    let env =  mfEnv st

    IteratedId name scripts <- getSessionData `onNothing` return (IteratedId noid mempty)
    let r =  lookup ("auto"++name) env
    if r == Nothing || (name == noid && newAsk st== True)
     then return $ FormElm((ftag "span" render) `attrs` [("id",id)]) mx
     else do
       setSessionData $ IteratedId name $ scripts <> "setId('"++id++"','" ++ toString (toByteString $ render)++"');"
       return $ FormElm mempty mx

noid= "noid"


-- | permits the edition of the rendering of a widget at run time. Once saved, the new rendering
-- becomes the new rendering of the widget for all the users. You must keep the active elements of the
-- template
--
-- the first parameter is the user that has permissions for edition. the second is a key that
-- identifies the template.
edTemplate
  :: (MonadIO m, FormInput v, Typeable a) =>
      UserStr -> Key -> View v m a -> View v m a
edTemplate muser k w=  View $ do
   nam     <- genNewId

   let ipanel= nam++"panel"
       name= nam++"-"++k
       install= "installEditField('"++muser++"','"++cookieuser++"','"++name++"','"++ipanel++"');\n"


   requires [JScript     installEditField
            ,JScriptFile nicEditUrl [install]
            ,JScript     ajaxSendText
            ,JScriptFile jqueryScript []
            ,ServerProc  ("_texts",  transient getTexts)]
   us <- getCurrentUser
   when(us== muser) noCache
   FormElm text mx <- runView w
   content <- liftIO $ readtField text k

   return $ FormElm (ftag "div" mempty `attrs` [("id",ipanel)] <>
                     ftag "span" content `attrs` [("id", name)])
                     mx
   where
   getTexts :: Token -> IO ()  -- low level server process
   getTexts token= do
     let (k,s):_ = tenv token
     liftIO $ do
       writetField k  $ (fromStrNoEncode s `asTypeOf` viewFormat w)
       flushCached k
       sendFlush token $ HttpData [] [] "" --empty response

       return()


   viewFormat :: View v m a -> v
   viewFormat= undefined -- is a type function

-- | Does the same than template but without the edition facility
template
  :: (MonadIO m, FormInput v, Typeable a) =>
      Key -> View v m a -> View v m a
template k w= View $ do
    FormElm text mx <- runView  w
    let content= unsafePerformIO $ readtField   text k
    return $ FormElm content mx



------------------- JQuery widgets -------------------
-- | present the JQuery datePicker calendar to choose a date.
-- The second parameter is the configuration. Use \"()\" by default.
-- See http://jqueryui.com/datepicker/
datePicker :: (Monad m, FormInput v) => String -> Maybe String -> View v m (Int,Int,Int)
datePicker conf jd= do
    id <- genNewId
    let setit= "$(document).ready(function() {\
                   \$( '#"++id++"' ).datepicker "++ conf ++";\
                \});"

    requires
      [CSSFile      jqueryCSS
      ,JScriptFile  jqueryScript []
      ,JScriptFile  jqueryUI [setit]]

    s <- getString jd <! [("id",id)]
    let (month,r) = span (/='/')  s
    let (day,r2)= span(/='/') $ tail r
    return (read day,read month, read $ tail r2)

-- | present a jQuery dialog with a widget. When a button is pressed it return the result.
-- The first parameter is the configuration. To make it modal,  use \"({modal: true})\" see  <http://jqueryui.com/dialog/> for
-- the available configurations.
--
-- The enclosed widget will be wrapped within a form tag if the user do not encloses it using wform.f
wdialog :: (Monad m, FormInput v) => String -> String -> View v m a -> View v m a
wdialog conf title w= do
    id <- genNewId
    let setit= "$(document).ready(function() {\
                   \$('#"++id++"').dialog "++ conf ++";\
                   \var idform= $('#"++id++" form');\
                   \idform.submit(function(){$(this).dialog(\"close\")})\
                \});"

    modify $ \st -> st{needForm= HasForm}
    requires
      [CSSFile      jqueryCSS
      ,JScriptFile  jqueryScript []
      ,JScriptFile  jqueryUI [setit]]

    (ftag "div" <<< insertForm w) <! [("id",id),("title", title)]





-- | Capture the form or link submissions and send them via jQuery AJAX.
-- The response is the new presentation of the widget, that is updated. No new page is generated
-- but the functionality is equivalent. Only the activated widget rendering is  updated
-- in the client, so a widget with autoRefresh can be used in heavyweight pages.
-- If AJAX/JavaScript are not available, the widget is refreshed normally, via a new page.
--
-- autoRefresh encloses the widget in a form tag if  it includes form elements.
--
-- If there are more than one autoRefresh, they must be enclosed within 'pageFlow' elements
autoRefresh
  :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
autoRefresh =  update "html"

-- | In some cases, it is necessary that a link or form inside a 'autoRefresh' or 'update' block
-- should not be autorefreshed, since it produces side effects in the rest of the page that
-- affect to the rendering of the whole. If you like to refresh the whole page, simply add
-- noAutoRefresh attribute to the widget to force the refresh of the whole page when it is activated.
--
-- That behavior is common at the last sentence of the 'autoRefresh' block.
--
-- This is a cascade menu example.
--
-- > r <- page $ autoRefresh $ ul <<< do
-- >        li <<< wlink OptionA << "option A"
-- >        ul <<< li <<< (wlink OptionA1 << "Option A1" <! noAutoRefresh)
-- >           <|> li <<< (wlink OptionA2 << "Option A2" <! noAutoRefresh)
-- >        <|>...
-- >           maybe other content
-- >
-- > case r of
-- >    OptionA1 -> pageA1
-- >    OptionA2 -> pageA2
--
-- when @option A@ is clicked, the two sub-options appear with autorefresh. Only the two
-- lines are returned by the server using AJAX. but when Option A1-2 is pressed we want to
-- present other pages, so we add the noAutorefresh attribute.
--
-- NOTE: the noAutoRefresh attribute should be added to the <a/> or <form/> tags.
noAutoRefresh= [("class","_noAutoRefresh")]

-- | does the same than `autoRefresh` but append the result of each request to the bottom of the widget
--
-- all the comments and remarks of `autoRefresh` apply here
appendUpdate  :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
appendUpdate= update "append"

-- | does the same than `autoRefresh` but prepend the result of each request before the current widget content
--
-- all the comments and remarks of `autoRefresh` apply here
prependUpdate   :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
prependUpdate= update "prepend"

update :: (MonadIO m, FormInput v)
  => String
  -> View v m a
  -> View v m a
update method w= do
    id <- genNewId
    st <- get

    let t = mfkillTime st -1

        installscript=
            "$(document).ready(function(){\
                 \ajaxGetLink('"++id++"');\
                 \ajaxPostForm('"++id++"');\
                 \});"
    st <- get
    let insync =  inSync st
    let env= mfEnv st
    let r= lookup ("auto"++id) env
    if r == Nothing
      then do
         requires [JScript $ timeoutscript t
                  ,JScript ajaxGetLink
                  ,JScript ajaxPostForm
                  ,JScriptFile jqueryScript [installscript]]
         (ftag "div" <<< insertForm w) <! [("id",id)]

      else refresh $ fromStr (method <> " ") ++> insertForm w
--        View $ do
--         let t= mfToken st            -- !> "JUST"
--         modify $ \s -> s{mfHttpHeaders=[]} -- !> "just"
--         resetCachePolicy
--         FormElm form mr <- runView $ insertForm w
--         setCachePolicy
--         st' <- get
--         let HttpData ctype c s= toHttpData $ method <> " " <> toByteString  form
--
--         (liftIO . sendFlush t $ HttpData (ctype ++
--                                mfHttpHeaders st') (mfCookies st' ++ c) s)
--         put st'{mfAutorefresh=True,newAsk=True}
--
--         return $ FormElm mempty Nothing

  where
  -- | adapted from http://www.codeproject.com/Articles/341151/Simple-AJAX-POST-Form-and-AJAX-Fetch-Link-to-Modal
--           \url: actionurl+'?bustcache='+ new Date().getTime()+'&auto'+id+'=true',\n\
  ajaxGetLink = "\nfunction ajaxGetLink(id){\
    \var id1= $('#'+id);\
    \var ida= $('#'+id+' a[class!=\"_noAutoRefresh\"]');\
    \ida.off('click');\
    \ida.click(function () {\
    \if (hadtimeout == true) return true;\
    \var pdata = $(this).attr('data-value');\
    \var actionurl = $(this).attr('href');\
    \var dialogOpts = {\
           \type: 'GET',\
           \url: actionurl+'?auto'+id+'=true',\
           \data: pdata,\
           \success: function (resp) {\
                \var ind= resp.indexOf(' ');\
                \var dat= resp.substr(ind);\
                \var method= resp.substr(0,ind);\
                \if(method== 'html')id1.html(dat);\
                \else if (method == 'append') id1.append(dat);\
                \else if (method == 'prepend') id1.prepend(dat);\
                \else $(':root').html(resp);\
                \ajaxGetLink(id);\
                \ajaxPostForm(id);\
           \},\
           \error: function (xhr, status, error) {\
               \var msg = $('<div>' + xhr + '</div>');\
               \id1.html(msg);\
           \}\
       \};\
    \$.ajax(dialogOpts);\
    \return false;\
    \});\
  \}\n"

  ajaxPostForm = "\nfunction ajaxPostForm(id) {\
    \var buttons= $('#'+id+' input[type=\"submit\"]');\
    \var idform= $('#'+id+' form[class!=\"_noAutoRefresh\"]');\
    \buttons.off('click');\
    \buttons.click(function(event) {\
      \if ($(this).attr('class') != '_noAutoRefresh'){\
        \event.preventDefault();\
        \if (hadtimeout == true) return true;\
        \var $form = $(this).closest('form');\
        \var url = $form.attr('action');\
        \pdata = 'auto'+id+'=true&'+this.name+'='+this.value+'&'+$form.serialize();\
        \postForm(id,url,pdata);\
        \return false;\
        \}else {\
          \noajax= true;\
          \return true;\
        \}\
     \})\
    \\n\
    \var noajax;\
    \idform.submit(function(event) {\
     \if(noajax) {noajax=false; return true;}\
       \event.preventDefault();\
       \var $form = $(this);\
       \var url = $form.attr('action');\
       \var pdata = 'auto'+id+'=true&' + $form.serialize();\
       \postForm(id,url,pdata);\
       \return false;})\
    \}\
    \function postForm(id,url,pdata){\
        \var id1= $('#'+id);\
        \$.ajax({\
            \type: 'POST',\
            \url: url,\
            \data: pdata,\
            \success: function (resp) {\
                \var ind= resp.indexOf(' ');\
                \var dat = resp.substr(ind);\
                \var method= resp.substr(0,ind);\
                \if(method== 'html')id1.html(dat);\
                \else if (method == 'append') id1.append(dat);\
                \else if (method == 'prepend') id1.prepend(dat);\
                \else $(':root').html(resp);\
                \ajaxGetLink(id);\
                \ajaxPostForm(id);\
            \},\
            \error: function (xhr, status, error) {\
                \var msg = $('<div>' + xhr + '</div>');\
                \id1.html(msg);\
            \}\
        \});\
       \};"




timeoutscript t=
     "\nvar hadtimeout=false;\
     \if("++show t++" > 0)setTimeout(function() {hadtimeout=true; }, "++show (t*1000)++");\n"


data UpdateMethod= Append | Prepend | Html deriving Show

-- | continuously execute a widget and update the content.
-- The update method specify how the update is done. 'Html' means a substitution of content.
-- The second parameter is the delay for the next retry in case of disconnection, in milliseconds.
--
-- It can be used to show data updates in the server. The widget is executed in a different process than
--  the one of the rest of the page.
-- Updates in the session context are not seen by the push widget. It has his own context.
-- To communicate with the widget, use DBRef's or TVar and the
-- STM semantics for waiting updates using 'retry'.
--
-- Widgets in a push can have links and forms, but since they are asynchronous, they can not
-- return inputs. but they can modify the server state.
-- push ever return an invalid response to the calling widget, so it never
-- triggers the advance of the navigation.
--
--
-- This example is a counter increased each second:
--
-- > pushIncrease= do
-- >   tv <- liftIO $ newTVarIO 0
-- >   page $ push 0 Html $ do
-- >       n <- atomic $ readTVar tv
-- >       atomic $ writeTVar tv $ n + 1
-- >       liftIO $ threadDelay 1000000
-- >       b << (show n) ++> noWidget
--
--
-- This other  simulates a console output that echoes what is entered in a text box
-- below. It has two widgets: a push output in append mode and a text box input.
-- The communication uses a TVar. The push widget wait for updates in the TVar.
-- because the second widget uses autoRefresh, all happens in the same page.
--
-- It is recommended to add a timeout to the push widget, like in the example:
--
-- >  pushSample=  do
-- >   tv <- liftIO $ newTVarIO $ Just "init"
-- >   page $ push Append 1000 (disp tv) <** input tv
-- >
-- >   where
-- >   disp tv= do
-- >       setTimeouts 100 0
-- >       line <- tget tv
-- >       p <<  line ++> noWidget
-- >
-- >   input tv= autoRefresh $ do
-- >       line <- getString Nothing <** submitButton "Enter"
-- >       tput tv line
-- >
-- >   tput tv x = atomic $ writeTVar  tv ( Just x)  !> "WRITE"
-- >
-- >   tget tv= atomic $ do
-- >       mr <- readTVar tv
-- >       case mr of
-- >          Nothing -> retry
-- >          Just r -> do
-- >           writeTVar tv Nothing
-- >           return r

push :: FormInput v
  => UpdateMethod
  -> Int
  -> View v IO ()
  -> View v IO ()
push method' wait w= push' . map toLower $ show method'
 where
 push' method= do
    id <- genNewId
    st <- get
    let token= mfToken st

        procname= "_push" ++ tind token ++ id
        installscript=
            "$(document).ready(function(){\n"
               ++ "ajaxPush('"++id++"',"++show wait++");"
               ++ "})\n"

    new <- gets newAsk

    when new  $ do
        killWF procname token{twfname= procname}
        let proc=runFlow . transientNav . ask $ w'
        requires [ServerProc (procname, proc),
                  JScript $ ajaxPush procname,
                  JScriptFile jqueryScript [installscript]]

    (ftag "div" <<< noWidget) <! [("id",id)]
      <++ ftag "div" mempty `attrs` [("id",id++"status")]

   where
   w' = do
     modify $ \s -> s{inSync= True,newAsk=True}
     w



   ajaxPush procname=" function ajaxPush(id,waititime){\
    \var cnt=0; \
    \var id1= $('#'+id);\
    \var idstatus= $('#'+id+'status');\
    \var ida= $('#'+id+' a');\
       \var actionurl='/"++procname++"';\
       \var dialogOpts = {\
           \cache: false,\
           \type: 'GET',\
           \url: actionurl,\
           \data: '',\
           \success: function (resp) {\
             \idstatus.html('');\
             \cnt=0;\
             \id1."++method++"(resp);\
             \ajaxPush1();\
           \},\
           \error: function (xhr, status, error) {\
                \cnt= cnt + 1;\
                \if  (false) \
                   \idstatus.html('no more retries');\
                \else {\
                   \idstatus.html('waiting');\
                   \setTimeout(function() { idstatus.html('retrying');ajaxPush1(); }, waititime);\
                \}\
           \}\
       \};\
    \function ajaxPush1(){\
       \$.ajax(dialogOpts);\
       \return false;\
     \}\
     \ajaxPush1();\
  \}"




-- | show the jQuery spinner widget. the first parameter is the configuration . Use \"()\" by default.
-- See http://jqueryui.com/spinner
getSpinner
  :: (MonadIO m, Read a,Show a, Typeable a, FormInput view) =>
     String -> Maybe a -> View view m a
getSpinner conf mv= do
    id <- genNewId
    let setit=   "$(document).ready(function() {\
                 \var spinner = $( '#"++id++"' ).spinner "++conf++";\
                 \spinner.spinner( \"enable\" );\
                 \});"
    requires
      [CSSFile      jqueryCSS
      ,JScriptFile  jqueryScript []
      ,JScriptFile  jqueryUI [setit]]

    getTextBox mv <! [("id",id)]



-- | takes as argument a widget and delay the load until it is visible. The rendering to
-- be shown during the load is the specified in the first parameter. The resulting lazy
-- widget behaves programatically in the same way.
--
-- It can lazily load recursively. It means that if the loaded widget has a lazy statement,
-- it will be honored as well.
--
-- Because a widget can contain arbitrary HTML, images or javascript, lazy can be used to lazy
-- load anything.
--
-- To load a image:
--
-- lazy temprendering $ wraw ( img ! href imageurl)
--
-- or
--
-- lazy temprendering $ img ! href imageurl ++> noWidget
lazy :: (FormInput v,Functor m,MonadIO m) => v -> View v m a -> View v m a
lazy v w=  do
    id <- genNewId
    st <- get
    let path = currentPath st
        env = mfEnv st
        r= lookup ("auto"++id) env
        t = mfkillTime st -1
        installscript =  "$(document).ready(function(){\
                \function lazyexec(){lazy('"++id++"','"++ path ++"',lazyexec)};\
                \$(window).one('scroll',lazyexec);\
                \$(window).trigger('scroll');\
                 \});"

    if r == Nothing  then View $ do
      requires [JScript lazyScript
                  ,JScriptFile jqueryScript [installscript,scrollposition]]
      reqs <- gets mfRequirements
      FormElm _ mx <- runView w
      modify $ \st-> st{mfRequirements= reqs}  --ignore requirements
      return $ FormElm (ftag "div" v `attrs` [("id",id)]) mx

     else refresh w

    where

    scrollposition= "$.fn.scrollposition= function(){\
       \var pos= $(this).position();\
       \if (typeof(pos)==='undefined') {return 1;}\
       \else{\
         \return pos.top - $( window ).scrollTop() - $( window ).height();\
         \}};"

    lazyScript= "function lazy (id,actionurl,f) {\
     \var now = new Date().getTime(),\
         \id1= $('#'+id),\
         \lastCall= 0;\
     \diff = now - lastCall;\
     \if (diff < 5000) {\
        \$(window).one('scroll',f);}\
     \else {\
      \lastCall = now;\
    \if(id1.scrollposition() > 0){\
    \$(window).one('scroll',f);}\
    \else{\
    \var dialogOpts = {\
           \type: 'GET',\
           \url: actionurl+'?auto'+id+'=true',\
           \success: function (resp) {\
                \id1.html(resp);\
                \$(window).trigger('scroll');\
           \},\
           \error: function (xhr, status, error) {\
               \var msg = $('<div>' + xhr + '</div>');\
               \id1.html(msg);\
           \}\
       \};\
    \$.ajax(dialogOpts);\
    \}}};"

refresh w=  View $ do
         resetCachePolicy
         modify $ \st -> st{mfAutorefresh=True,inSync= True}
         FormElm form mx <- runView w   -- !> show (mfInstalledScripts st')
         setCachePolicy
         st' <- get
         let t= mfToken st'
         reqs <- installAllRequirements
         let HttpData ctype c s= toHttpData $ toByteString  form
         liftIO . sendFlush t $ HttpData (ctype ++
                                mfHttpHeaders st') (mfCookies st' ++ c)
                              $ s <> toByteString reqs
         return $ FormElm mempty mx

--waitAndExecute= "function waitAndExecute(sym,f) {\
--        \if (eval(sym)) {f();}\
--          \else {setTimeout(function() {waitAndExecute(sym,f)}, 50);}\
--        \}\n"
