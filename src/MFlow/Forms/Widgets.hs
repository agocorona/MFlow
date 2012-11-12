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
{-# LANGUAGE UndecidableInstances,ExistentialQuantification, FlexibleInstances, OverlappingInstances, DeriveDataTypeable #-}
{-
 translate userWidgets
 userFormOrName

to do
grid
selectio ajax

-}
module MFlow.Forms.Widgets (selectAutocomplete, tField, tFieldEd, tFieldGen

) where
import MFlow
import MFlow.Forms
import MFlow.Forms.Ajax
import MFlow.Forms.XHtml
import Text.XHtml
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.Trans
import Data.Typeable
import Data.List
import System.IO.Unsafe
import Debug.Trace
import Control.Monad.State
import Data.TCache
import Data.TCache.Defs
import Data.TCache.Memoization
import Data.RefSerialize
import qualified Data.Map as M
import Data.IORef
import MFlow.Cookies
import Data.Persistent.IDynamic

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
--  "var fileref=document.createElement('script');\
--  \fileref.setAttribute('type','text/javascript');\
--  \var text= document.createTextNode(\""++content++"\");\
--  \fileref.appendChild(text);\
--  \document.getElementsByTagName('head')[0].appendChild(fileref);"


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


data WebRequirement= CSSFile String
                | CSS String
                | JScriptFile String [String]
                | JScript String
                | ServerProc (String, Token -> Workflow IO ())
                deriving (Eq, Ord, Typeable)

instance Eq    (String, Token -> Workflow IO ()) where
   (x,_) == (y,_)= x == y

instance Ord    (String, Token -> Workflow IO ()) where
   compare(x,_)  (y,_)= compare x y

instance Requirements WebRequirement where
   installRequirements= installWebRequirements



installWebRequirements ::  (Monad m,FormInput view) =>[WebRequirement] ->    m view
installWebRequirements rs= do
  let s =  aggregate  $ sort rs
  return $ ftag "script" (fromStrNoEncode s) `addAttributes` [("type","text/javascript")]
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



selectAutocomplete :: (MonadIO m,Functor m)=> (String -> IO [String]) -> View Html m [String]
selectAutocomplete serverproc = do

    let events=
                 "$(document).ready(function(){   \
                 \  $('#text1').keydown(function(){ \
                 \   if(event.keyCode == 13)  \
                 \    {var v= $('#text1').attr('value'); \
                 \      event.preventDefault(); \
                 \     $('#users').html(function(i,orig){ return (orig + '<div><input type=checkbox value='+v+'/>'+v+' <div>')}); \
                 \     $('[type=checkbox]').parent().click(function(){$(this).remove()}); \
                 \    } \
                 \ }); \
                 \});"
    addRequirements [JScript ajaxScript , JScriptFile jqueryScript [events], CSSFile jqueryCSS
               , JScriptFile jqueryUi []]
    let jaddtoautocomp us= "$('#text1').autocomplete({ source: " ++ show us ++ "  });"
    ajaxc <- ajaxCommand "$('#text1').attr('value')"
                         $ \u -> do
                                 r <- serverproc u
                                 return $ jaddtoautocomp r



    getCheckBoxes
              (thediv ! [strAttr "id" "users"]
               <<< noWidget )


     <++ input ![thetype "text"
                ,value "select users"
                ,strAttr "id" "text1"
                ,strAttr "oninput" ajaxc
                ,strAttr "autocomplete" "off"]



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

tFieldEd
  :: (Functor m,  MonadIO m, Executable m,
      Typeable v, FormInput v, ToByteString v) =>
      Key -> v -> View v m ()
tFieldEd  k text=
   tFieldGen k  (readtField text) writetField



tFieldGen :: (MonadIO m,Functor m, Executable m
        , Typeable v, FormInput v,ToByteString v)
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
            let nikeditor= "var myNicEditor = new nicEditor();"
            let callback=useradmin ++
                  "bkLib.onDomLoaded(function() {\
                     \    myNicEditor.addInstance('"++name++"');\
                     \});"

            ajaxjs <- ajaxCommand
                        ("'"++k++ "'+','+ document.getElementById('"++name++"').innerHTML")
                        $ \str -> do
                          let (k,s)= break (==',')    str
                          liftIO  . create  k  $ fromStrNoEncode (tail s)
                          flushCached k
                          return "alert('saved');"

            addRequirements
                   [JScriptFile "http://js.nicedit.com/nicEdit-latest.js" [nikeditor, callback]
                   ,JScript ajaxScript]

            return [("id", name),("ondblclick",  useradmin++ajaxjs)]

    wraw $  (ftag "span" content `addAttributes` attribs)

tField :: (MonadIO m,Functor m, Executable m
        , Typeable v, FormInput v,ToByteString v)
        => Key
        -> View v m ()
tField  k    =  wfreeze k 0 $ do
    content <-  liftIO $ readtField (fromStrNoEncode "not found")  k
    wraw content

