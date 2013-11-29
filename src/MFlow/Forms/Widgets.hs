
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
autoRefresh, noAutoRefresh, appendUpdate, prependUpdate, push, UpdateMethod(..)

-- * JQueryUi widgets
,datePicker, getSpinner, wautocomplete, wdialog,

-- * User Management
userFormOrName,maybeLogout, wlogin,

-- * Active widgets
wEditList,wautocompleteList
, wautocompleteEdit,

-- * Editing widgets
delEdited, getEdited,prependWidget,appendWidget,setWidget

-- * Content Management
,tField, tFieldEd, htmlEdit, edTemplate, dField, template, witerate,tfieldKey

-- * Multilanguage
,mFieldEd, mField

-- * utility
,insertForm


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
import Unsafe.Coerce
import Control.Exception


readyJQuery="ready=function(){if(!window.jQuery){return setTimeout(ready,100)}};"

jqueryScript= "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
jqueryScript1="//code.jquery.com/jquery-1.9.1.js"

jqueryCSS1= "//code.jquery.com/ui/1.9.1/themes/base/jquery-ui.css"
jqueryCSS= "//code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"

jqueryUI1= "//code.jquery.com/ui/1.9.1/jquery-ui.js"
jqueryUI= "//code.jquery.com/ui/1.10.3/jquery-ui.js"

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

-- | If not logged, it present a page flow which askm  for the user name, then the password if not logged
--
-- If logged, it present the user name and a link to logout
--
-- normally to be used with autoRefresh and pageFlow when used with other widgets.
wlogin :: (MonadIO m,Functor m,FormInput v) => View v m ()
wlogin= insertForm $ do
   username <- getCurrentUser
   if username /= anonymous
         then return username
         else do
          name <- getString Nothing <! hint "login name" <! size 9 <++ ftag "br" mempty
          pass <- getPassword <! hint "password" <! size 9
                     <++ ftag "br" mempty
                     <** submitButton "login" 
          val  <- userValidate (name,pass)
          case val of
            Just msg -> notValid msg
            Nothing  -> login name >> return name
       
   `wcallback` (\name -> ftag "b" (fromStr $ "logged as " ++ name)
                     ++>  wlink ("logout" :: String) (ftag "b" $ fromStr " logout"))
   `wcallback`  const (logout >> wlogin)
   
focus = [("onload","this.focus()")]
hint s= [("placeholder",s)]
size n= [("size",show n)]

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

--       where

--       des1 _ bs=
--          let s= B.unpack bs -- read . B.unpack
--          in case s of
--               ('T':'F':'i':'e':'l':'d':' ':s)  ->
--                  let
--                      [(k,rest)] =  readsPrec 0 s
--                      [(content,_)] = readsPrec 0 $ tail rest
--                  in TField k (B.pack content)
--               _ -> error "not match"
    setPersist =   \_ -> Just filePersist


--applyDeserializers [] k str = x where
--     x= error $ "can not deserialize "++ B.unpack str++" to type: "++ show (typeOf x)
--
--applyDeserializers (d:ds) k str=  unsafePerformIO $
--      (return $! d k str) `catch` (\(_ :: SomeException)-> return (applyDeserializers ds k str))


writetField k s= atomically $ writeDBRef (getDBRef k) $ TField k $ toByteString s


readtField text k= atomically $ do
   let ref = getDBRef k
   mr <- readDBRef ref
   case mr of
    Just (TField k v) -> if v /= mempty then return $ fromStrNoEncode $ B.unpack v else return text
    Nothing -> return text


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

nicEditUrl= "//js.nicedit.com/nicEdit-latest.js"


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
       install= "\ninstallEditField('"++muser++"','"++cookieuser++"','"++name++"','"++ipanel++"');\n"
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
--            ,JScriptFile jqueryScript []
            ,ServerProc  ("_texts",  transient getTexts)]

   (ftag "div" mempty `attrs` [("id",ipanel)]) ++>
    notValid (ftag "span" content `attrs` [("id", name)])



installEditField=
          "\nfunction installEditField(muser,cookieuser,name,ipanel){\n\
            \if(muser== '' || document.cookie.search(cookieuser+'='+muser) != -1)\n\
                 \ bkLib.onDomLoaded(function() {\n\
                 \   var myNicEditor = new nicEditor({fullPanel : true, onSave : function(content, id, instance) {\
                 \        ajaxSendText(id,content);\n\
                 \        myNicEditor.removeInstance(name);\n\
                 \        myNicEditor.removePanel(ipanel);\n\
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
        \return false;\n\
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

newtype IteratedId= IteratedId  String deriving Typeable

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
witerate  w= do
   name  <- genNewId
   setSessionData $ IteratedId name
   st <- get
   let index= mfPIndex st
   let t= mfkillTime st
   let installAutoEval=
        "$(document).ready(function(){\n\
           \autoEvalLink('"++name++"','"++ show index ++"');\
           \autoEvalForm('"++name++"');\
           \})\n"
   let r = lookup ("auto"++name) $ mfEnv st
   ret <- case r of
    Nothing -> do
     requires [JScript     autoEvalLink
              ,JScript     autoEvalForm
              ,JScript     setId
              ,JScript     $ timeoutscript t
              ,JScriptFile jqueryScript [installAutoEval]]

     (ftag "div" <<< insertForm w) <! [("id",name)]


    Just sind -> View $ do
         let t= mfToken st
         let index= read sind
         put st{mfPIndex= index}
         modify $ \s -> s{mfRequirements=[]}

         FormElm _ mr <- runView  w

         reqs <- return . map ( \(Requirement r) -> unsafeCoerce r) =<< gets mfRequirements
         let js = jsRequirements reqs
         liftIO . sendFlush t $ HttpData
                                (("Cache-Control", "no-cache, no-store"):mfHttpHeaders st)
                                (mfCookies st) (B.pack js)
         modify $ \st -> st{mfAutorefresh=True,inSync=True}
         return $ FormElm [] mr

   delSessionData $ IteratedId name
   return ret

autoEvalLink = "\nfunction autoEvalLink(id,ind){\n\
    \var id1= $('#'+id);\n\
    \var ida= $('#'+id+' a[class!=\"_noAutoRefresh\"]');\n\
    \ida.click(function () {\n\
    \ if (hadtimeout == true) return true;\n\
    \ var pdata = $(this).attr('data-value');\n\
    \ var actionurl = $(this).attr('href');\n\
    \ var dialogOpts = {\n\
    \       type: 'GET',\n\
    \       url: actionurl+'?bustcache='+ new Date().getTime()+'&auto'+id+'='+ind,\n\
    \       data: pdata,\n\
    \       success: function (resp) {\n\
    \           eval(resp);\n\
    \       },\n\
    \       error: function (xhr, status, error) {\n\
    \           var msg = $('<div>' + xhr + '</div>');\n\
    \           id1.html(msg);\n\
    \       }\n\
    \   };\n\
    \ $.ajax(dialogOpts);\n\
    \ return false;\n\
    \});\n\
  \}\n"

autoEvalForm = "\nfunction autoEvalForm(id) {\n\
    \var id1= $('#'+id);\n\
    \var idform= $('#'+id+' form[class!=\"_noAutoRefresh\"]');\n\
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
                \eval(resp);\n\
            \},\n\
            \error: function (xhr, status, error) {\n\
                \var msg = $('<div>' + xhr + '</div>');\n\
                \id1.html(msg);\n\
            \}\n\
        \});\n\
       \});\n\
      \return false;\n\
     \}\n"

setId= "function setId(id,v){document.getElementById(id).innerHTML= v;};\n"

-- Present a widget via AJAX if it is within a 'witerate' context. In the first iteration it present the
-- widget surrounded by a placeholder. subsequent iterations will send just the javascript code
-- necessary for the refreshing of the placeholder.
dField
  :: (Monad m, FormInput view) =>
     View view m  b -> View view m b
dField w= View $ do
    id <- genNewId
    FormElm vs mx <- runView w
    let render = mconcat vs
    st <- get
    let env =  mfEnv st

    IteratedId name <- getSessionData `onNothing` return (IteratedId noid)
    let r =  lookup ("auto"++name) env
    if r == Nothing || (name == noid && newAsk st== True)  then do
       requires [JScriptFile jqueryScript ["$(document).ready(function() {setId('"++id++"','" ++ B.unpack (toByteString $ render)++"')});\n"]]
       return $ FormElm[(ftag "span" render) `attrs` [("id",id)]] mx
     else do
       requires [JScript $  "setId('"++id++"','" ++ B.unpack (toByteString $ render)++"');\n"]
       return $ FormElm mempty mx

noid= "noid"

--edTemplateList
--  :: (MonadIO m, Functor m, Typeable b, FormInput view) =>
--     UserStr -> String -> (a ->View view m  b) -> [a] -> View view m b
--edTemplateList user  name w xs= View $ do
--    id <- genNewId
--    let ws= Prelude.map w xs
--    FormElm vx mx <- runView $
--          edTemplate user name ( Prelude.head ws) <|>
--          firstOf [template name w | w <- Prelude.tail ws]
--
--    let render = mconcat vx
--    st <- get
--    let t= mfkillTime st
--    let env =  mfEnv st
--    let insertResults= "insert('" ++ id ++ "',"++ show (B.unpack $ toByteString render) ++");"
--    IteratedId name <- getSessionData `onNothing` return (IteratedId noid)
--    let r =  lookup ("auto"++name) env           -- !> ("TIMEOUT="++ show t)
--    if r == Nothing || (name == noid && newAsk st== True)
--     then do
--         requires[JScript autoEvalLink
--                 ,JScript $ timeoutscript t
--                 ,JScript jsInsertList
--                 ,JScriptFile jqueryScript [insertResults]]
--         return $ FormElm[(ftag "span" render) `attrs` [("id",id)]] mx  !> "NOPARAM edTemplateList"
--     else do
--       modify $ \st -> st{mfRequirements= [],mfAutorefresh=True,inSync=True}
--       requires[JScript insertResults]
--             !> "VALIDATED edTemplateList"
--       return $ FormElm mempty mx
--

-- | load a template with the name equal to the second parameter in the texts folder. If no template
-- exist, it uses the widget rendering. If the first parameter match the name of the logged user,
-- the template will be editable at runtime. edTemplate will present an edition bar on the top of
-- the template. The changes in the template will be effective inmediately for all the users.
--
-- The return value is the one returned by the internal widget each time it is executed.
--
-- edTemplate can be used to enrich the content and layout of a widget, for example, by adding
-- extra links, text and formatting. Widgets with form fields work well with an 'edTemplate' mask
-- as long as the tags created by the widget are not deleted, but the validation messages will not appear
--
-- To add dynamic elements to the template for data presentation and//or input field validation
-- messages, combine it with 'witerate' and 'dField'
edTemplate
  :: (MonadIO m, FormInput v, Typeable a) =>
      UserStr -> Key -> View v m a -> View v m a
edTemplate muser k w=  View $ do
   nam     <- genNewId

   let ipanel= nam++"panel"
       name= nam++"-"++k
       install= "\ninstallEditField('"++muser++"','"++cookieuser++"','"++name++"','"++ipanel++"');\n"


   requires [JScriptFile nicEditUrl [install]
            ,JScript     ajaxSendText
            ,JScript     installEditField
            ,JScriptFile jqueryScript []
            ,ServerProc  ("_texts",  transient getTexts)]

   FormElm text mx <- runView w
   content <- liftIO $ readtField (mconcat text) k

   return $ FormElm [ftag "div" mempty `attrs` [("id",ipanel)]
                    ,ftag "span" content `attrs` [("id", name)]]
                     mx
   where
   getTexts :: (Token -> IO ())
   getTexts token= do
     let (k,s):_ = tenv token
     liftIO $ do
       writetField k  $ (fromStrNoEncode s `asTypeOf` viewFormat w)
       flushCached k
       sendFlush token $ HttpData [] [] ""
       return()

   viewFormat :: View v m a -> v
   viewFormat= undefined -- is a type function

-- | Does the same than template but without the edition facility
template k w= View $ do
    FormElm text mx <- runView  w
    let content= unsafePerformIO $ readtField  (mconcat text) k
    return $ FormElm [content] mx

--edTemplateList
--  :: (Typeable a,FormInput view) =>
--     UserStr -> String  -> [View view Identity a] -> View view IO a
--edTemplateList user templ  ws=  do
--  let  id = templ
--  let wrapid=  "wrapper-" ++ id
--  text <- liftIO $ readtField  (ftag "div" mempty `attrs` [("id",wrapid)])  wrapid
--  let   vwrtext= B.unpack $ toByteString (text `asTypeOf` witness ws)
--
----  wrapperEd wrapid vwrtext **>
--  (ftag "div" <<< elems id wrapid vwrtext) <! [("id",wrapid)]
--  where
--  witness :: [View view Identity a] -> view
--  witness = undefined
--
--  elems id wrapid vwrtext= View $ do
--    let holder=  ftag "span" (fromStr "holder") `attrs` [("_holder",id)]
--
--    FormElm fedit _ <- runView $ tFieldEd user templ holder
--    frest  <- liftIO $ readtField holder  templ
--
--    forms <-  mapM (runView . changeMonad) ws
--    let vs   = map (\(FormElm v _) ->  mconcat v) forms
--    requires [JScriptFile jqueryScript
--              [
----               replacewrap
----              ,"replacewrap('"++ wrapid ++ "','" ++ vwrtext ++ "')",
--              jsInsertList
--              ,"$(document).ready(function() {\
--               \insert('" ++ id ++ "',"++ show (map ( B.unpack . toByteString) vs) ++");\n\
--               \});"]]
--
--    let res = filter isJust $ map (\(FormElm _ r) -> r) forms
--        res1= if null res then Nothing else head res
--
--    return $ FormElm  (mconcat fedit: take (length vs -1) (repeat frest)) res1
--
jsInsertList =
      "\nfunction insert(id,vs){\n\
      \$('[_holder=\"'+id+'\"]').each(function(n,it) {\n\
      \  $(it).html(vs[n]);\n\
      \})};\n"

--  wrapperEd :: (FormInput view) => String -> view -> View view IO ()
--  wrapperEd  wrapid  wrtext = do -- autoRefresh . pageFlow "wrap" $ do
--        vwrtext <- liftIO $ readtField  (ftag "div" mempty `attrs` [("id",wrapid)])  wrapid
--        let wrtext= B.unpack $ toByteString (vwrtext `asTypeOf` witness)
--        nwrtext<- getString  (Just wrtext)
--                          <! [("id", wrapid++"-ed")]
--                   <** submitButton "OK"
--
--        liftIO $ writetField wrapid (fromStr nwrtext `asTypeOf` witness ws) !> nwrtext


--  replacewrap = "\nfunction replacewrap(id,wrapcode){\n\
--          \   var selector= '#'+id;\n\
--          \   var children = $(selector).children();\n\
--          \   var nchildren;\n\
--          \   $(selector).replaceWith(wrapcode);\n\
--          \   $(selector).html(children);\n\
--          \   if(wrapcode.search('table') != -1)\n\
--          \        $(children[1].children).wrap('<tr><td></td></tr>')\n\
--          \};\n"

--          \   if(wrapcode.search('xx') != -1){\n\
--          \        children.each(function(n,it){\n\
--          \                      $(it).wrap('<li></li>')});\n\

--          \        })\n\



------------------- JQuery widgets -------------------
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

-- | In some cases, it is neccessary that a link or form inside a 'autoRefresh' or 'update' block
-- should not be autorefreshed, since it produces side effects in the rest of the page that
-- affect to the rendering of the whole. If you like to refresh the whole page, simply add
-- noAutoRefresh attribute to the widget to force the refresh of the whole page when it is activated.
--
-- That behaviour is common at the last sentence of the 'autoRefresh' block.
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

update :: (MonadIO m,
     FormInput v)
  => B.ByteString
  -> View v m a
  -> View v m a
update method w= do
    id <- genNewId
    st <- get
    let t = mfkillTime st -1

    let installscript=
            "$(document).ready(function(){\n"
               ++ "ajaxGetLink('"++id++"');"
               ++ "ajaxPostForm('"++id++"');"
               ++ "})\n"

    let r= lookup ("auto"++id) $ mfEnv st           -- !> ("TIMEOUT="++ show t)
    case r of
      Nothing -> do
         requires [JScript $ timeoutscript t
                  ,JScript ajaxGetLink
                  ,JScript ajaxPostForm
                  ,JScriptFile jqueryScript [installscript]]
         (ftag "div" <<< insertForm w) <! [("id",id)] 

      Just sind -> View $ do
         let t= mfToken st
         FormElm form mr <- runView $ insertForm w
         st <- get
         let HttpData ctype c s= toHttpData $ method <> " " <> toByteString (mconcat form)
         liftIO . sendFlush t $ HttpData (ctype ++ ("Cache-Control", "no-cache, no-store"):mfHttpHeaders st) (mfCookies st ++ c) s
         put st{mfAutorefresh=True,inSync=True}
         return $ FormElm [] mr

  where


  -- | adapted from http://www.codeproject.com/Articles/341151/Simple-AJAX-POST-Form-and-AJAX-Fetch-Link-to-Modal
  ajaxGetLink = "\nfunction ajaxGetLink(id){\n\
    \var id1= $('#'+id);\n\
    \var ida= $('#'+id+' a[class!=\"_noAutoRefresh\"]');\n\
    \ida.click(function () {\n\
    \if (hadtimeout == true) return true;\n\
    \var pdata = $(this).attr('data-value');\n\
    \var actionurl = $(this).attr('href');\n\
    \var dialogOpts = {\n\
    \       type: 'GET',\n\
    \       url: actionurl+'?bustcache='+ new Date().getTime()+'&auto'+id+'=true',\n\
    \       data: pdata,\n\
    \       success: function (resp) {\n\
    \            var ind= resp.indexOf(' ');\n\
    \            var dat = resp.substr(ind);\n\
    \            var method= resp.substr(0,ind);\n\
    \            if(method== 'html')id1.html(dat);\n\
    \            else if (method == 'append') id1.append(dat);\n\
    \            else id1.prepend(dat);\n\
    \            ajaxGetLink(id);\n\
    \       },\n\
    \       error: function (xhr, status, error) {\n\
    \           var msg = $('<div>' + xhr + '</div>');\n\
    \           id1.html(msg);\n\
    \       }\n\
    \   };\n\
    \$.ajax(dialogOpts);\n\
    \return false;\n\
    \});\n\
  \}\n"

  ajaxPostForm = "\nfunction ajaxPostForm(id) {\n\
    \var id1= $('#'+id);\n\
    \var idform= $('#'+id+' form[class!=\"_noAutoRefresh\"]');\n\
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
    \            var ind= resp.indexOf(' ');\n\
    \            var dat = resp.substr(ind);\n\
    \            var method= resp.substr(0,ind);\n\
    \            if(method== 'html')id1.html(dat);\n\
    \            else if (method == 'append') id1.append(dat);\n\
    \            else id1.prepend(dat);\n\
    \            ajaxPostForm(id);\n\
            \},\n\
            \error: function (xhr, status, error) {\n\
                \var msg = $('<div>' + xhr + '</div>');\n\
                \id1.html(msg);\n\
            \}\n\
        \});\n\
       \});\n\
      \return false;\n\
     \}\n"

timeoutscript t=
     "\nvar hadtimeout=false;\n\
     \if("++show t++" > 0)setTimeout(function() {hadtimeout=true; }, "++show (t*1000)++");\n"


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
    \            if  (false) \n\
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

--    let r= lookup ("auto"++id) $ mfEnv st           -- !> ("TIMEOUT="++ show t)
--    case r of
--      Nothing -> do
--         requires [
--                  JScript $ ajaxPush method,
--                  JScriptFile jqueryScript [installscript]]
--         (ftag "div" <<< insertForm w) <! [("id",id)]  
--
--      Just sind -> View $ do
--         let t= mfToken st
--         FormElm form mr <- runView $ insertForm w
--         st <- get
--         let HttpData ctype c s= toHttpData $ B.pack method <> " " <> toByteString (mconcat form)
--         liftIO . sendFlush t $ HttpData (ctype ++ ("Cache-Control", "no-cache, no-store"):mfHttpHeaders st) (mfCookies st ++ c) s
--         put st{mfAutorefresh=True,inSync=True}
--         return $ FormElm [] mr
--
-- ajaxPush method =" function ajaxPush(id,verb,waititime){\n\
--    \var cnt=0; \n\
--    \var id1= $('#'+id);\n\
--    \var idstatus= $('#'+id+'status');\n\
--    \var ida= $('#'+id+' a');\n\
--    \var actionurl = './';\n\
--    \var dialogOpts = {\n\
--    \       cache: false,\n\
--    \       type: 'GET',\n\
--    \       url: actionurl+'?bustcache='+ new Date().getTime()+'&auto'+id+'=true',\n\
--    \       data: '',\n\
--    \       success: function (resp) {\n\
--    \         idstatus.html('')\n\
--    \         cnt=0;\n\
--    \         id1."++method++"(resp);\n\
--    \         ajaxPush1();\n\
--    \       },\n\
--    \       error: function (xhr, status, error) {\n\
--    \            cnt= cnt + 1;\n\
--    \            if  (false) \n\
--    \               idstatus.html('no more retries');\n\
--    \            else {\n\
--    \               idstatus.html('waiting');\n\
--    \               setTimeout(function() { idstatus.html('retrying');ajaxPush1(); }, waititime);\n\
--    \            }\n\
--    \       }\n\
--    \   };\n\
--    \function ajaxPush1(){\n\
--    \   $.ajax(dialogOpts);\n\
--    \   return false;\n\
--    \ }\n\
--    \ ajaxPush1();\n\
--  \}"
--



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




