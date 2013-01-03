-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Form.Widgets
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
{-# LANGUAGE UndecidableInstances,ExistentialQuantification
            , FlexibleInstances, OverlappingInstances
            , OverloadedStrings, DeriveDataTypeable #-}
{-


to do
grid
  widget visualiser

-}
module MFlow.Forms.Widgets (userFormOrName,maybeLogout,wEditList,selectAutocomplete, selectAutocompleteEdit, tField, tFieldEd, tFieldGen
,requires, WebRequirement(..)

) where
import MFlow
import MFlow.Forms
import MFlow.Forms.Ajax
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
import Control.Monad.Identity

import Debug.Trace
(!>)= flip trace

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

readyJQuery="ready=function(){if(!window.jQuery){return setTimeout(ready,100)}};"

jqueryScript= "http://ajax.googleapis.com/ajax/libs/jquery/1.8.0/jquery.min.js"

jqueryCSS= "http://code.jquery.com/ui/1.9.1/themes/base/jquery-ui.css"

jqueryUi= "http://code.jquery.com/ui/1.9.1/jquery-ui.js"

type Script= String
type OnLoadScript= String
type File= String
data WebRequirement= JScriptFile File [OnLoadScript]
                   | CSSFile String
                   | CSS Script
                   | JScript Script
                   | ServerProc (String, Flow)
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


{-
  como intercalar ajax commands en View

  ask $ do
          x <- widget
          v <- get jQuery
          setHTML selector html
          setWidget selector widget
          if x
             then widget2
             else widget3

ajax command envia y recibe
     si no es ajax return()
     al volver, comienza desde el principio.
         correcto.


-}
--newtype AjaxSessionId= AjaxSessionId String deriving Typeable
--
--installCommands :: MonadIO m => (String -> View v m ()) -> View v m (String -> String)
--installCommands f= do
--    id <- genNewId
--    installServerControl id $ \s -> do
--          setSessionData $ AjaxSessionId id
--          f s
--          end
--    where
--    end=do
--      t<- getToken
--      liftIO $ sendFlush t ""




--type Selector= String
--type Event= String
--installHandler:: (MonadIO m) => Selector -> Event -> View v m JSCommand  -> View v m (AjaxResult ())
--installHandler selector event cmd= do
--   id <- genNewId
--   setSessionData $ AjaxSessionId id
--   t <- getToken
--   AjaxRes s <- command $ return $ JSC $"$('"++selector++"')."++event++"(function(){"++readEvalLoop t id "''"++"};"
--   return $ s `asTypeOf` ""
--   command cmd


------- User Management ------
userFormOrName  mode wid= userWidget mode wid `wmodify` f  <** maybeLogout
  where
  f _ justu@(Just u)  =  return ([fromStr u], justu) -- !> "input"
  f felem Nothing = do
     us <- getCurrentUser -- getEnv cookieuser
     if us == anonymous
           then return (felem, Nothing)
           else return([fromStr us],  Just us)

maybeLogout :: (MonadIO m,Functor m,FormInput v) => View v m ()
maybeLogout= do
    us <- getCurrentUser
    if us/= anonymous
      then fromStr " " ++> wlink () (fromStr "logout") `waction` const logout
      else noWidget


data Medit view m a = Medit (M.Map B.ByteString [(String,View view m a)])
instance (Typeable view, Typeable a)
         =>Typeable (Medit view m a) where
  typeOf= \v -> mkTyConApp (mkTyCon "Medit" )
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
    Medit stored <-  getSessionData `onNothing` return (Medit (M.empty))
    return $ fromMaybe [] $ M.lookup id stored

getEdited id= do
  r <- getEdited1 id
  let (k,ws)= unzip r
  return ws

delEdited id witness=do
    (ks,ws) <- return . unzip =<< getEdited1 id
    return $ ws `asTypeOf` witness
    mapM (liftIO . flushCached) ks
    setEdited id ([] `asTypeOf` (zip (repeat "") witness))

setEdited id ws= do
    Medit stored <-  getSessionData `onNothing` return (Medit (M.empty))
    let stored'= M.insert id ws stored
    setSessionData . Medit $ stored'


addEdited id w= do
    ws <- getEdited1 id
    setEdited id (w:ws)

{-
cambios:
ajax que incluya comando para enviar un widget
dos opciones:
  pasivo: crea el widget y lo almacena en una lista conocida con un id
              solo puede ser parametrizado por la llamada Ajax.
              puede incluir un paramtro (string -> IO widget) que es llamado
              para generar el widget
  reactivo: espera una TVar por un widget y lo envia.
              puede ser llamado con varios widgets y contenidos

  necesito el reactivo
    porque necesito pasar diferentes paramentros.
-}
modifyWidget :: (MonadIO m,Executable m,Typeable a,FormInput v)
           => B.ByteString -> B.ByteString -> View v Identity a -> View v m B.ByteString
modifyWidget  selector modifier  w = View $ do
     ws <- getEdited selector
     let n =  length (ws `asTypeOf` [w])
     let key= "widget"++ show selector ++  show n
     let cw = wcached key 0  w
     addEdited selector (key,cw)
     FormElm form _ <-  runView cw
     let elem=  toByteString  $ mconcat form
     return . FormElm [] . Just $   selector <> "." <> modifier <>"('" <> elem <> "');"

prependWidget sel w= modifyWidget sel "prepend" w
appendWidget sel w= modifyWidget sel "append" w
setWidget sel w= modifyWidget sel "html" w


wEditList :: (Typeable a,Read a
             ,FormInput view
             ,Functor m,MonadIO m, Executable m)
          => (view ->view)
          -> (Maybe String -> View view Identity a)
          -> [String] -> View view m  [a]
wEditList holderview w xs = do

    let ws=  map (w . Just) xs
        wn=  w Nothing
    id1<- genNewId
    let sel= "$('#"<>  B.pack id1 <> "')"
    callAjax <- ajax . const $ prependWidget sel wn
    let installevents= "$(document).ready(function(){\
              \$('#wEditListAdd').click(function(){"++callAjax "''"++"});})"

    requires [JScript ajaxScript, JScriptFile jqueryScript [installevents] ]

    ws' <- getEdited sel

    r <-  (holderview  <<< (manyOf $ ws' ++ map changeMonad ws)) <! [("id",id1)]

    delEdited sel ws'

    return  r


selectAutocompleteEdit
    :: (Typeable a, MonadIO m,Functor m, Executable m
     , FormInput v)
    => String
    -> (String -> IO [String])
    -> (Maybe String  -> View v Identity a)
    -> [String]
    -> View v m [a]
selectAutocompleteEdit phold   autocomplete  elem values= do
    id1 <- genNewId
    let sel= "$('#" <> B.pack id1 <> "')"
    ajaxc <- ajax $ \(c:u) ->
              case c of
                'f' -> prependWidget sel (elem $ Just u)
                _   -> do
                          r <- liftIO $ autocomplete u
                          return $ jaddtoautocomp r


    requires [JScript ajaxScript
             ,JScriptFile jqueryScript [events ajaxc] -- [events]
             ,CSSFile jqueryCSS
             ,JScriptFile jqueryUi []]

    ws' <- getEdited sel

    r<-(ftag "div" mempty  `attrs` [("id",  id1)]
      ++> manyOf (ws' ++ (map (changeMonad . elem . Just) values)))
      <++ ftag "input" mempty
             `attrs` [("type", "text")
                     ,("id", "text1")
                     ,("placeholder", phold)
                     ,("oninput",ajaxc "'n'+$('#text1').attr('value')" )
                     ,("autocomplete", "off")]
    delEdited sel ws'
    return r
    where
    events ajaxc=
         "$(document).ready(function(){   \
         \  $('#text1').keydown(function(){ \
         \   if(event.keyCode == 13){  \
             \   var v= $('#text1').attr('value'); \
             \   event.preventDefault();" ++
                 ajaxc "'f'+v"++";"++
             "   $('#text1').val('');\
         \  }\
         \ });\
         \});"

    jaddtoautocomp us= "$('#text1').autocomplete({ source: " <> B.pack( show us) <> "  });"

selectAutocomplete phold serverproc values=
 selectAutocompleteEdit phold serverproc  wrender1 values
 where
 wrender1 x= ftag "div"    <<< ftag "input" mempty
                                `attrs` [("type","checkbox")
                                        ,("checked","")
                                        ,("onclick","this.parentNode.parentNode.removeChild(this.parentNode)")]
                           ++> ftag "span" (fromStr $ fromJust x )
                           ++> whidden( fromJust x)

------- Templating and localization ---------

writetField    k s=  atomically $ writetFieldSTM k s

writetFieldSTM k s=  do
             phold <- readDBRef tFields `onNothing` return (M.fromList [])
             let r= M.insert k  (toByteString s) phold
             writeDBRef tFields   r

readtField text k= atomically $ do
       hs<- readDBRef tFields `onNothing` return (M.fromList [])
       let mp=  M.lookup k hs
       case mp of
         Just c  -> return   $ fromStrNoEncode $ B.unpack c
         Nothing -> writetFieldSTM k  text >> return  text

type TFields  = M.Map String B.ByteString
instance Indexable TFields where
    key _= "texts"
    defPath _= "texts/"

tFields :: DBRef TFields
tFields =  getDBRef "texts"

type Key= String

-- | A widget that display the content of an  html, But if logged as administrator,
-- it permits to edit it in place. So the editor could see the final appearance
-- of what he write in the page.
--
-- When the administrator double click in the paragraph, the content is saved and
-- identified by the key. Then, from now on the users will see the content of the saved
-- content.
--
-- The content is saved in a file by default ("texts" in this versions), but there is
-- a configurable version (`tFieldGen`). The content of the element and the formatting
-- is cached in memory, so the display is very fast.
tFieldEd
  :: (Functor m,  MonadIO m, Executable m,
      FormInput v) =>
      Key -> v -> View v m ()
tFieldEd  k text=
   tFieldGen k  (readtField text) writetField


-- tFieldEd with user-configurable storage.
tFieldGen :: (MonadIO m,Functor m, Executable m
        ,FormInput v)
        => Key
        -> (String -> IO  v)
        -> (String ->v  -> IO())

        -> View v m ()
tFieldGen  k  getcontent create =   wfreeze k 0 $ do
    content <-  liftIO $ getcontent  k
    admin  <- getAdminName
    attribs <- do
            name <- genNewId
            let useradmin= "if(document.cookie.search('"++cookieuser++"="++admin++"') != -1)"
                nikeditor= "var myNicEditor = new nicEditor();"
                callback=useradmin ++
                  "bkLib.onDomLoaded(function() {\
                     \    myNicEditor.addInstance('"++name++"');\
                     \});"
                param= ("'"++k++ "'+','+ document.getElementById('"++name++"').innerHTML")
            ajaxjs <- ajax
                        $ \str -> do
                          let (k,s)= break (==',')    str
                          liftIO  . create  k  $ fromStrNoEncode (tail s)
                          liftIO $ flushCached k
                          return "alert('saved');"

            requires
                   [JScriptFile "http://js.nicedit.com/nicEdit-latest.js" [nikeditor, callback]
                   ,JScript ajaxScript]

            return [("id", name),("ondblclick",  useradmin++ajaxjs param)]

    wraw $  (ftag "span" content `attrs` attribs)

tField :: (MonadIO m,Functor m, Executable m
       ,  FormInput v)
       => Key
       -> View v m ()
tField  k    =  wfreeze k 0 $ do
    content <-  liftIO $ readtField (fromStrNoEncode "not found")  k
    wraw content

-- | a multilanguage version of tFieldEd. For a field with @key@ it add a suffix with the
-- two characters of the language used.
mFieldEd k content= do
  lang <- getLang
  tFieldEd (k ++ ('-':lang)) content

mField k= do
  lang <- getLang
  tField $ k ++ ('-':lang)
