

-- | A very simple (but effective) support for AJAX.
-- The value of a javaScript variable is sent to the server.
-- The server must return a valid  sequence of javaScript statements
-- that are evaluated in the client.
--
-- This example increase the value, from 0 on, in a text box trough AJAX:
--
-- @
-- ajaxsample= do
--
--   let ajaxf n= return $ "document.getElementById('text1').value='"++show(read  n +1)++"'"
--   ajaxc <- ajaxCommand "document.getElementById('text1').value" ajaxf
--
--   ask $  requires[JScript ajaxScript]
--       >> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc)]
--   breturn()@
--
-- here `requires` install the ajaxScript in the browser


module MFlow.Forms.Ajax (ajaxCommand, ajaxScript) where
import MFlow
import MFlow.Forms
import Text.XHtml
import Control.Monad.Trans
--import Data.ByteString.Lazy.Char8
import Data.RefSerialize (newContext,addrHash)
import Data.IORef
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.State
import Data.Map (keys)
import qualified Data.CaseInsensitive as CI

--import Debug.Trace
--(!>)= flip trace

context= unsafePerformIO newContext

-- | Install the server code and return the client code for an AJAX interaction.
ajaxCommand :: MonadIO m
            =>  String                -- ^ The javScript expression whose value will be sent to the server
            -> (String -> IO String)  -- ^ The server procedure to be executed with the
                                      -- variable value as parameter. It must return a string with valid
                                      -- javaScript code to be executed in the client
            ->  m (String)            -- ^ return the javascript of the event handler, to be inserted in
                                      --  the HTML to be sent to the client
ajaxCommand jsparam serverProc = do
   r <- liftIO $ addrHash context serverProc
   servname <- case r of
                Left h -> do
                     let servname= "ajax"++ show h
                     liftIO $! addMessageFlows [( servname,  serverp)]
                     return servname
                Right h -> return $ "ajax"++ show h
--   liftIO $ getMessageFlows >>= return . keys >>=  print
   return $ ajaxCall jsparam servname
   where
   serverp = stateless $ \env -> do
        let c = lookup "ajax" env   `justify`  (error "not found ajax command") -- :: String
        serverProc c
   justify = flip  fromMaybe
   ajaxCall jsparam servname = "doServer("++"'" ++  servname++"',"++jsparam++")"




-- | the ajax script necessary for ajax execution.
--
-- must be inserted in the widget with `requires`
--
-- > ask $ requires[ajaxScript] >> widget-with-ajax-call

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
        "function doServer (servproc,param){" ++
        "   xmlhttp.open('GET',servproc+'?ajax='+param,true);" ++
        "   xmlhttp.send();};" ++
        ""++
        "xmlhttp.onreadystatechange=function()" ++
        "  {" ++
        "  if (xmlhttp.readyState + xmlhttp.status==204)" ++
        "    { " ++
        "    eval(xmlhttp.responseText);" ++
        "    }" ++
        "  };" ++
        ""

