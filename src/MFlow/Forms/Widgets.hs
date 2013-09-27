
{- |
Some dynamic widgets, widgets that dynamically edit content in other widgets,
widgets for templating, content management and multilanguage. And some primitives
to create other active widgets.
-}

{-# LANGUAGE UndecidableInstances,ExistentialQuantification
            , FlexibleInstances, OverlappingInstances, FlexibleContexts
            , OverloadedStrings, DeriveDataTypeable , ScopedTypeVariables
            , TemplateHaskell #-}




module MFlow.Forms.Widgets (
-- * Ajax refreshing of widgets
autoRefresh, appendUpdate, prependUpdate, push, UpdateMethod(..)

-- * JQueryUi widgets
,datePicker, getSpinner, wautocomplete, wdialog,

-- * User Management
userFormOrName,maybeLogout,

-- * Active widgets
wEditList,wautocompleteList
, wautocompleteEdit,

-- * Editing widgets
delEdited, getEdited
,prependWidget,appendWidget,setWidget

-- * Content Management
,tField, tFieldEd, htmlEdit

-- * Multilanguage
,mFieldEd, mField

) where
import MFlow
import MFlow.Forms
import MFlow.Forms.Internals
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as B
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



readyJQuery="ready=function(){if(!window.jQuery){return setTimeout(ready,100)}};"

jqueryScript1= "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
jqueryScript="http://code.jquery.com/jquery-1.9.1.js"

jqueryCSS1= "http://code.jquery.com/ui/1.9.1/themes/base/jquery-ui.css"
jqueryCSS= "http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"

jqueryUI1= "http://code.jquery.com/ui/1.9.1/jquery-ui.js"
jqueryUI= "http://code.jquery.com/ui/1.10.3/jquery-ui.js"

------- User Management ------

-- | Present a user form if not logged in. Otherwise, the user name and a logout link is presented.
-- The paremeters and the behaviour are the same as 'userWidget'.
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
instance (Typeable view, Typeable a)
         =>Typeable (Medit view m a) where
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

getEdited1 id= do
    Medit stored <- getSessionData `onNothing` return (Medit M.empty)
    return $ fromMaybe [] $ M.lookup id stored

-- | Return the list of edited widgets (added by the active widgets) for a given identifier
getEdited
  :: (Typeable v, Typeable a, MonadState (MFlowState view) m) =>
     B.ByteString -> m [View v m1 a]
getEdited id= do
  r <- getEdited1 id
  let (_,ws)= unzip r
  return ws

-- | Deletes the list of edited widgets for a certain identifier and with the type of the witness widget parameter
delEdited
  :: (Typeable v, Typeable a, MonadIO m,
      MonadState (MFlowState view) m)
     => B.ByteString           -- ^ identifier
     -> [View v m1 a] -> m ()  -- ^ withess
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


modifyWidget :: (MonadIO m,Executable m,Typeable a,FormInput v)
           => B.ByteString -> B.ByteString -> View v Identity a -> View v m B.ByteString
modifyWidget selector modifier  w = View $ do
     ws <- getEdited selector
     let n =  length (ws `asTypeOf` [w])
     let key= "widget"++ show selector ++  show n ++ show (typeOf $ typ w)
     let cw = wcached key 0  w
     addEdited selector (key,cw)
     FormElm form _ <-  runView cw
     let elem=  toByteString  $ mconcat form
     return . FormElm [] . Just $   selector <> "." <> modifier <>"('" <> elem <> "');"
     where
     typ :: View v Identity a -> a
     typ = undefined
-- | Return the javascript to be executed on the browser to prepend a widget to the location
-- identified by the selector (the bytestring parameter), The selector must have the form of a jquery expression
-- . It stores the added widgets in the edited list, that is accessed with 'getEdited'
--
-- The resulting string can be executed in the browser. 'ajax' will return the code to
-- execute the complete ajax roundtrip. This code returned by ajax must be in an eventhabdler.
--
-- This example  will insert a widget in the div  when the element with identifier
-- /clickelem/  is clicked. when the form is sbmitted, the widget values are returned
-- and the list of edited widgets are deleted.
--
-- >    id1<- genNewId
-- >    let sel= "$('#" <>  B.pack id1 <> "')"
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
  :: (Typeable a, MonadIO m, Executable m, FormInput v)
  => B.ByteString           -- ^ jquery selector
  -> View v Identity a      -- ^ widget to prepend
  -> View v m B.ByteString  -- ^ string returned with the jquery string to be executed in the browser
prependWidget sel w= modifyWidget sel "prepend" w

-- | Like 'prependWidget' but append the widget instead of prepend.
appendWidget
  :: (Typeable a, MonadIO m, Executable m, FormInput v) =>
     B.ByteString -> View v Identity a -> View v m B.ByteString
appendWidget sel w= modifyWidget sel "append" w

-- | L  ike 'prependWidget' but set the entire content of the selector instead of prepending an element
setWidget
  :: (Typeable a, MonadIO m, Executable m, FormInput v) =>
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
             ,Functor m,MonadIO m, Executable m)
          => (view ->view)     -- ^ The holder tag
          -> (Maybe String -> View view Identity a) -- ^ the contained widget, initialized  by a string
          -> [String]          -- ^ The initial list of values.
          -> String            -- ^ The id of the button or link that will create a new list element when clicked
          -> View view m  [a]
wEditList holderview w xs addId = do
    let ws=  map (w . Just) xs
        wn=  w Nothing
    id1<- genNewId
    let sel= "$('#" <>  B.pack id1 <> "')"
    callAjax <- ajax . const $ prependWidget sel wn
    let installevents= "$(document).ready(function(){\
              \$('#"++addId++"').click(function(){"++callAjax "''"++"});})"

    requires [JScriptFile jqueryScript [installevents] ]

    ws' <- getEdited sel

    r <-  (holderview  <<< (manyOf $ ws' ++ map changeMonad ws)) <! [("id",id1)]
    delEdited sel ws'
    return r

--wpush
--  :: (Typeable a,
--      FormInput v) =>
--     (v -> v)
--     -> String
--     -> String
--     -> String
--     -> (String -> View v IO a)
--     -> View v IO a
--wpush  holder modifier addId expr w = do
--    id1 <- genNewId
--    let sel= "$('#" <>  B.pack id1 <> "')"
--    callAjax <- ajax $ \s ->  appendWidget sel ( changeMonad $ w s)
--    let installevents= "$(document).ready(function(){\
--              \$('#"++addId++"').click(function(){"++callAjax expr ++ "});})"
--
--    requires [JScriptFile jqueryScript [installevents] ]
--
--    ws <- getEdited sel
--
--    r <-  holder  <<< firstOf ws  <! [("id",id1)]
--    delEdited sel ws
--    return r



-- | Present the JQuery autocompletion list, from a procedure defined by the programmer, to a text box.
wautocomplete
  :: (Show a, MonadIO m, FormInput v)
  => Maybe String       -- ^ Initial value
  -> (String -> IO a)   -- ^ Autocompletion procedure: will receive a prefix and return a list of strings
  -> View v m String
wautocomplete mv autocomplete  = do
    text1 <- genNewId
    ajaxc <- ajax $ \u -> do
                          r <- liftIO $ autocomplete u
                          return $ jaddtoautocomp text1 r


    requires [JScriptFile jqueryScript [] -- [events]
             ,CSSFile jqueryCSS
             ,JScriptFile jqueryUI []]


    getString mv <!  [("type", "text")
                     ,("id", text1)
                     ,("oninput",ajaxc $ "$('#"++text1++"').attr('value')" )
                     ,("autocomplete", "off")]


    where
    jaddtoautocomp text1 us= "$('#"<>B.pack text1<>"').autocomplete({ source: " <> B.pack( show us) <> "  });"


-- | Produces a text box. It gives a autocompletion list to the textbox. When return
-- is pressed in the textbox, the box content is used to create a widget of a kind defined
-- by the user, which will be situated above of the textbox. When submitted, the result is the content
-- of the created widgets (the validated ones).
--
-- 'wautocompleteList' is an specialization of this widget, where
-- the widget parameter is fixed, with a checkbox that delete the eleement when unselected
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
     , FormInput v)
    => String                                 -- ^ the initial text of the box
    -> (String -> IO [String])                -- ^ the autocompletion procedure: receives a prefix, return a list of options.
    -> (Maybe String  -> View v Identity a)   -- ^ the widget to add, initialized with the string entered in the box
    -> [String]                               -- ^ initial set of values
    -> View v m [a]                           -- ^ resulting widget
wautocompleteEdit phold autocomplete  elem values= do
    id1 <- genNewId
    let textx= id1++"text"
    let sel= "$('#" <> B.pack id1 <> "')"
    ajaxc <- ajax $ \(c:u) ->
              case c  of
                'f' -> prependWidget sel (elem $ Just u)
                _   -> do
                          r <- liftIO $ autocomplete u
                          return $ jaddtoautocomp textx r


    requires [JScriptFile jqueryScript  [events textx ajaxc]
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
         \  $('#"++textx++"').keydown(function(){ \
         \   if(event.keyCode == 13){  \
             \   var v= $('#"++textx++"').val(); \
             \   if(event.preventDefault) event.preventDefault();\
             \   else if(event.returnValue) event.returnValue = false;" ++
                 ajaxc "'f'+v"++";"++
             "   $('#"++textx++"').val('');\
         \  }\
         \ });\
         \});"

    jaddtoautocomp textx us= "$('#"<>B.pack textx<>"').autocomplete({ source: " <> B.pack( show us) <> "  });"

-- | A specialization of 'wutocompleteEdit' which make appear each chosen option with
-- a checkbox that deletes the element when uncheched. The result, when submitted, is the list of selected elements.
wautocompleteList
  :: (Functor m, MonadIO m, Executable m, FormInput v) =>
     String -> (String -> IO [String]) -> [String] -> View v m [String]
wautocompleteList phold serverproc values=
 wautocompleteEdit phold serverproc  wrender1 values
 where
 wrender1 x= ftag "div"    <<< ftag "input" mempty
                                `attrs` [("type","checkbox")
                                        ,("checked","")
                                        ,("onclick","this.parentNode.parentNode.removeChild(this.parentNode)")]
                           ++> ftag "span" (fromStr $ fromJust x )
                           ++> whidden( fromJust x)

------- Templating and localization ---------
type Key= String
data TField  = TField Key B.ByteString  deriving (Read, Show,Typeable)

instance Indexable TField where
    key (TField k _)= k
    defPath _= "texts/"


instance Serializable TField where
    serialize= B.pack . show
    deserialize= read . B.unpack
--    setPersist = const $  Just filePersist

writetField k s= atomically $ writeDBRef (getDBRef k) $ TField k $ toByteString s


readtField text k= atomically $ do
   let ref = getDBRef k
   mr <- readDBRef ref
   case mr of
    Just (TField k v) -> return $ fromStrNoEncode $ B.unpack v
    Nothing -> do
        writeDBRef ref  $ TField k $ toByteString text
        return text


htmlEdit :: (Monad m, FormInput v) =>  [String] -> UserStr -> View v m a -> View v m a
htmlEdit buttons jsuser w = do
  id <- genNewId

  let installHtmlField=
          "\nfunction installHtmlField(muser,cookieuser,name,buttons){\n\
            \if(muser== '' || document.cookie.search(cookieuser+'='+muser) != -1)\n\
                 \ bkLib.onDomLoaded(function() {\n\
                 \   var myNicEditor = new nicEditor({buttonList : buttons});\n\
                 \   myNicEditor.panelInstance(name);\n\
                 \})};\n"
      install= "installHtmlField('"++jsuser++"','"++cookieuser++"','"++id++"',"++show buttons++");\n"

  requires [JScriptFile nicEditUrl [installHtmlField,install]]
  w <! [("id",id)]

nicEditUrl= "http://js.nicedit.com/nicEdit-latest.js"


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
tFieldEd  muser k text= wfreeze k 0 $  do
   content <- liftIO $ readtField text k
   nam     <- genNewId
   let ipanel= nam++"panel"
       name= nam++"-"++k
       install= "installEditField('"++muser++"','"++cookieuser++"','"++name++"','"++ipanel++"');\n"
       getTexts :: (Token -> IO ())
       getTexts token= do
         let (k,s):_ = tenv token
         liftIO $ do
           writetField k  $ (fromStrNoEncode s `asTypeOf` content)
           flushCached k
           sendFlush token $ HttpData [] [] ""
           return()

   requires [JScriptFile nicEditUrl [install]
            ,JScript     ajaxSendText
            ,JScript     installEditField
            ,JScriptFile jqueryScript []
            ,ServerProc  ("_texts",  transient getTexts)]

   (ftag "div" mempty `attrs` [("id",ipanel)]) ++>
    wraw (ftag "span" content `attrs` [("id", name)])

   where

   installEditField=
          "\nfunction installEditField(muser,cookieuser,name,ipanel){\n\
            \if(muser== '' || document.cookie.search(cookieuser+'='+muser) != -1)\n\
                 \ bkLib.onDomLoaded(function() {\n\
                 \   var myNicEditor = new nicEditor({fullPanel : true, onSave : function(content, id, instance) {\
                 \        ajaxSendText(id,content);\n\
                 \      }});\n\
                 \   myNicEditor.addInstance(name);\n\
                 \   myNicEditor.setPanel(ipanel);\n\
                 \})};\n"

   ajaxSendText = "\nfunction ajaxSendText(id,content){\n\
        \var arr= id.split('-');\n\
        \var k= arr[1];\n\
        \$.ajax({\n\
        \       type: 'POST',\n\
        \       url: '/_texts',\n\
        \       data: k + '='+ encodeURIComponent(content),\n\
        \       success: function (resp) {},\n\
        \       error: function (xhr, status, error) {\n\
        \                var msg = $('<div>' + xhr + '</div>');\n\
        \                id1.html(msg);\n\
        \       }\n\
        \   });\n\
        \alert ('saved');\n\
        \return false;\n\
        \};\n"



-- | Read the cached field value and present it without edition.
tField :: (MonadIO m,Functor m, Executable m
       ,  FormInput v)
       => Key
       -> View v m ()
tField  k    =  wfreeze k 0 $ do
    content <-  liftIO $ readtField (fromStrNoEncode "not found")  k
    wraw content

-- | A multilanguage version of tFieldEd. For a field with @key@ it add a suffix with the
-- two characters of the language used.
mFieldEd  muser k content= do
  lang <- getLang
  tFieldEd  muser (k ++ ('-':lang)) content

-- | A multilanguage version of tField
mField k= do
  lang <- getLang
  tField $ k ++ ('-':lang)

-- | present the JQuery datepicker calendar to choose a date.
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
-- The first parameter is the configuration. To make it modal,  use \"({modal: true})\" see  "http://jqueryui.com/dialog/" for
-- the available configurations.
--
-- As in the case of 'autoRefresh' the enclosed widget will be wrapped within a form tag if the user do not encloses it using wform.

wdialog :: (Monad m, FormInput v) => String -> String -> View v m a -> View v m a
wdialog conf title w= do
    id <- genNewId
    let setit= "$(document).ready(function() {\n\
                   \$('#"++id++"').dialog "++ conf ++";\n\
                   \var idform= $('#"++id++" form');\n\
                   \idform.submit(function(){$(this).dialog(\"close\")})\n\
                \});"

    modify $ \st -> st{needForm= False}
    requires
      [CSSFile      jqueryCSS
      ,JScriptFile  jqueryScript []
      ,JScriptFile  jqueryUI [setit]]

    (ftag "div" <<< insertForm w) <! [("id",id),("title", title)]



insertForm w=View $ do
    FormElm forms mx <- runView w
    st <- get


    cont <- case needForm st of
                      True ->  do
                               frm <- formPrefix (mfPIndex st) (twfname $ mfToken st ) st forms False
                               return   frm
                      _    ->  return $ mconcat  forms
    put st{needForm= False}
    return $ FormElm [cont] mx



-- | Capture the form submissions and the links of the enclosed widget and send them via jQuery AJAX.
-- The response is the new presentation of the widget, that is updated. No new page is generated
-- but the functionality is equivalent. Only the activated widget is executed in the server and updated
-- in the client, so a widget with autoRefresh can be used in heavyweight pages.
-- If AJAX/JavaScript are not available, the widget is refreshed normally, via a new page.
-- If has form elements, the enclosed widget must be a complete form and it must include the form action tag.
-- For this purpose, autoRefresh encloses the widget in a form tag if there are form elements on it
-- and the programmer has not enclosed them in a 'wform' element.
autoRefresh
  :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
autoRefresh w=  update "html" w

-- | does the same than autoRefresh but append the result of each request to the bottom of the widget
appendUpdate  :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
appendUpdate= update "append"

-- | does the same than autoRefresh but prepend the result of each request before the current widget content
prependUpdate   :: (MonadIO m,
     FormInput v)
  => View v m a
  -> View v m a
prependUpdate= update "prepend"

update method w= do
    id <- genNewId
    st <- get
    let t = mfkillTime st -1

    let installscript=
            "$(document).ready(function(){\n"
               ++ "ajaxGetLink('"++id++"');"
               ++ "ajaxPostForm('"++id++"');"
               ++ "})\n"

    r <- getParam1 ("auto"++id) $ mfEnv st           -- !> ("TIMEOUT="++ show t)
    case r of
      NoParam -> do
         requires [JScript $ timeoutscript t
                  ,JScript ajaxGetLink
                  ,JScript ajaxPostForm
                  ,JScriptFile jqueryScript [installscript]]
         (ftag "div" <<< insertForm w) <! [("id",id)] 

      Validated (x :: String) -> View $ do
         let t= mfToken st
         FormElm form mr <- runView $ insertForm w
         st <- get
         let HttpData ctype c s= toHttpData $ mconcat form
         liftIO . sendFlush t $ HttpData (ctype ++ ("Cache-Control", "no-cache, no-store"):mfHttpHeaders st) (mfCookies st ++ c) s
         put st{mfAutorefresh=True}
         return $ FormElm [] mr

  where

  timeoutscript t=
     "\nvar hadtimeout=false;\n\
     \setTimeout(function() {hadtimeout=true; }, "++show (t*1000)++");\n"

  -- | adapted from http://www.codeproject.com/Articles/341151/Simple-AJAX-POST-Form-and-AJAX-Fetch-Link-to-Modal
  ajaxGetLink = "function ajaxGetLink(id){\n\
    \var id1= $('#'+id);\n\
    \var ida= $('#'+id+' a');\n\
    \ida.click(function () {\n\
    \if (hadtimeout == true) return true;\n\
    \var pdata = $(this).attr('data-value');\n\
    \var actionurl = $(this).attr('href');\n\
    \var dialogOpts = {\n\
    \       type: 'GET',\n\
    \       url: actionurl+'?bustcache='+ new Date().getTime()+'&auto'+id+'=true',\n\
    \       data: pdata,\n\
    \       success: function (resp) {\n\
    \         id1."++method++"(resp);\n\
    \         ajaxGetLink(id)\n\
    \       },\n\
    \       error: function (xhr, status, error) {\n\
    \           var msg = $('<div>' + xhr + '</div>');\n\
    \           id1.html(msg);\n\
    \       }\n\
    \   };\n\
    \$.ajax(dialogOpts);\n\
    \return false;\n\
    \});\n\
  \}"

  ajaxPostForm = "function ajaxPostForm(id) {\n\
    \var id1= $('#'+id);\n\
    \var idform= $('#'+id+' form');\n\
    \idform.submit(function (event) {\n\
        \if (hadtimeout == true) return true;\n\
        \event.preventDefault();\n\
        \var $form = $(this);\n\
        \var url = $form.attr('action');\n\
        \var pdata = $form.serialize();\n\
        \$.ajax({\n\
            \type: 'POST',\n\
            \url: url,\n\
            \data: 'auto'+id+'=true&'+pdata,\n\
            \success: function (resp) {\n\
                \id1."++method++"(resp);\n\
                \ajaxPostForm(id)\n\
            \},\n\
            \error: function (xhr, status, error) {\n\
                \var msg = $('<div>' + xhr + '</div>');\n\
                \id1.html(msg);\n\
            \}\n\
        \});\n\
       \});\n\
      \return false;\n\
     \}"

data UpdateMethod= Append | Prepend | Html deriving Show

-- | continously execute a widget and update the content.
-- The update method specify how the update is done. 'Html' means a substitution of content.
-- The second parameter is the delay for the next retry in case of disconnection, in milliseconds.
--
-- It can be used to show data updates in the server. The widget is executed in a different process than
--  the one of the rest of the page. Although the process is initiated with the session context,
-- updates in the session context are not seen by the push widget
-- To communicate with te widget, use DBRef's or TVar and the
-- STM semantics for waiting updates using 'retry'.
--
-- Widgets in a push can have links and forms, but since they are asunchonous, they can not
-- return validated inputs. but they can modify the server state.
-- push ever return invalid to the calling widget, so it never
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
-- The communication it uses a TVar. The push widget wait for updates in the TVar.
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
        dat= mfData st
        procname= "_push" ++ tind token ++ id
        installscript=
            "$(document).ready(function(){\n"
               ++ "ajaxPush('"++id++"',"++show wait++");"
               ++ "})\n"

    new <- gets newAsk

    when new  $ do
        killWF procname token{twfname= procname}
        let proc= transient . runFlow . ask $ w' dat
        requires [ServerProc (procname, proc),
                  JScript $ ajaxPush procname,
                  JScriptFile jqueryScript [installscript]]

    (ftag "div" <<< noWidget) <! [("id",id)]
      <++ ftag "div" mempty `attrs` [("id",id++"status")]

   where
   w' dat= do
     modify $ \s -> s{inSync= True,newAsk=True,mfData=dat}
     w


   ajaxPush procname=" function ajaxPush(id,waititime){\n\
    \var cnt=0; \n\
    \var id1= $('#'+id);\n\
    \var idstatus= $('#'+id+'status');\n\
    \var ida= $('#'+id+' a');\n\
    \   var actionurl='/"++procname++"';\n\
    \   var dialogOpts = {\n\
    \       cache: false,\n\
    \       type: 'GET',\n\
    \       url: actionurl,\n\
    \       data: '',\n\
    \       success: function (resp) {\n\
    \         idstatus.html('')\n\
    \         cnt=0;\n\
    \         id1."++method++"(resp);\n\
    \         ajaxPush1();\n\
    \       },\n\
    \       error: function (xhr, status, error) {\n\
    \            cnt= cnt + 1;\n\
    \            if (cnt > 6)\n\
    \               idstatus.html('no more retries');\n\
    \            else {\n\
    \               idstatus.html('waiting');\n\
    \               setTimeout(function() { idstatus.html('retrying');ajaxPush1(); }, waititime);\n\
    \            }\n\
    \       }\n\
    \   };\n\
    \function ajaxPush1(){\n\
    \   $.ajax(dialogOpts);\n\
    \   return false;\n\
    \ }\n\
    \ ajaxPush1();\n\
  \}"

-- | show the jQuery spinner widget. the first parameter is the configuration . Use \"()\" by default.
-- See http://jqueryui.com/spinner
getSpinner
  :: (MonadIO m, Read a,Show a, Typeable a, FormInput view) =>
     String -> Maybe a -> View view m a
getSpinner conf mv= do
    id <- genNewId
    let setit=   "$(document).ready(function() {\n\
                 \var spinner = $( '#"++id++"' ).spinner "++conf++";\n\
                 \spinner.spinner( \"enable\" );\n\
                 \});"
    requires
      [CSSFile      jqueryCSS
      ,JScriptFile  jqueryScript []
      ,JScriptFile  jqueryUI [setit]]

    getTextBox mv <! [("id",id)]




