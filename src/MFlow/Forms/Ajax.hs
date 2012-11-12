{-# OPTIONS  -XFlexibleContexts    #-}

-- | A very simple (but effective) support for AJAX.
-- The value of a javaScript variable is sent to the server.
-- The server must return a valid  sequence of javaScript statements
-- that are evaluated in the client.
--
-- This example increase the value, from 0 on, in a text box trough AJAX:
--
-- @
-- ajaxheader html= thehtml << `ajaxHead` << html
-- ajaxsample= do
--   setHeader ajaxheader
--   ajaxc \<- `ajaxCommand` \"document.getElementById(\'text1\').value\"
--                           (\n ->  return $ \"document.getElementById(\'text1\').value='\"++show(read n +1)++\"'\")
--   ask $ (getInt (Just 0) \<! [(\"id\",\"text1\"),(\"onclick\",ajaxc)])
--   breturn()@

module MFlow.Forms.Ajax (ajaxCommand,ajaxHead, ajaxScript) where
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

-- | @ajaxHead@ must be used instead of `header` when using ajax(see example).
--
-- Although it produces code form "Text.XHtml" rendering (package xhtml),
-- it can be converted to byteString, so that any rendering can be used trough normalization
-- . see `setHeader`. Example:
--
-- > setHeader $ \html -> thehtml << ajaxHead << p << "click the box" +++ html
ajaxHead :: Html -> Html
ajaxHead html= header <<  ajaxScript1 +++ body << html

-- | ajax script  included in `ajaxHead`
ajaxScript1 :: Html
ajaxScript1= script ![thetype "text/javascript"] <<  ajaxScript

--ajaxScript :: String
--ajaxScript=
--        "function loadXMLObj()\n" ++
--        "{\n" ++
--        "var xmlhttp;\n" ++
--        "if (window.XMLHttpRequest)\n" ++
--        "  {\n// code for IE7+, Firefox, Chrome, Opera, Safari\n" ++
--        "  xmlhttp=new XMLHttpRequest();\n" ++
--        "  }\n" ++
--        "else\n" ++
--        "  {\n// code for IE6, IE5\n\n" ++
--        "  xmlhttp=new ActiveXObject('Microsoft.XMLHTTP');\n" ++
--        "  }\n" ++
--        "return xmlhttp\n" ++
--        "}\n" ++
--
--        " xmlhttp= loadXMLObj();\n" ++
--        " noparam= '';\n"++
--        "\n"++
--        "function doServer (servproc,param){\n" ++
--        "   xmlhttp.open('GET',servproc+'?ajax='+param,true);\n" ++
--        "   xmlhttp.send();}\n" ++
--        "\n"++
--        "xmlhttp.onreadystatechange=function()\n" ++
--        "  {\n" ++
--        "  if (xmlhttp.readyState + xmlhttp.status==204)\n" ++
--        "    { \n" ++
--        "    eval(xmlhttp.responseText);\n" ++
--        "    }\n" ++
--        "  }\n" ++
--        "\n"


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

