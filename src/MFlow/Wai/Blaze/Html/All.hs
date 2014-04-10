----------------------------------------------------------------------------
--
-- Module      :  MFlow.Wai.Blaze.Html.All
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

module MFlow.Wai.Blaze.Html.All (
 module Data.TCache
,module MFlow
,module MFlow.Forms
,module MFlow.Forms.Widgets
,module MFlow.Forms.Blaze.Html
,module MFlow.Forms.Admin
,module Control.Applicative
,module Text.Blaze.Html5
,module Text.Blaze.Html5.Attributes
,module Control.Monad.IO.Class
,module MFlow.Forms.WebApi
,module MFlow.Forms.Cache
,runNavigation
,runSecureNavigation
) where

import MFlow
import MFlow.Wai
import MFlow.Forms
import MFlow.Forms.Widgets
import MFlow.Forms.Admin
import MFlow.Forms.Blaze.Html
import MFlow.Forms.WebApi
import MFlow.Forms.Cache
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes  hiding (label,span,style,cite,title,summary,step,form)
import Network.Wai
import Network.Wai.Handler.Warp(run,defaultSettings,Settings,setPort)
import Data.TCache
import Text.Blaze.Internal(text)

import Control.Workflow (Workflow, unsafeIOtoWF)


import Control.Applicative
import Control.Monad(when, unless)
import Control.Monad.IO.Class
import System.Environment
import Data.Maybe(fromMaybe)
import Data.Char(isNumber)
import Network.Wai.Handler.WarpTLS as TLS

-- | The port is read from the first exectution parameter.
-- If no parameter, it is read from the PORT environment variable.
-- if this does not exist, the port 80 is used.
getPort= do
    args <- getArgs
    port <- case args of
           port:xs -> return port
           _  -> do
               env <- getEnvironment
               return $ fromMaybe "80" $ lookup "PORT" env
    let porti= if and $ map isNumber port then fromIntegral $ read port
                                          else 80
    putStr "using port "
    print porti
    return porti

-- | run a persistent flow. It uses `getPort` to get the port
-- The first parameter is the first element in the URL path.
-- It also set the home page
runNavigation :: String -> FlowM Html (Workflow IO) () -> IO () 
runNavigation n f= do
    unless (null n) $ setNoScript n
    addMessageFlows[(n, runFlow f)]
    porti <- getPort
    wait $ run porti waiMessageFlow
    --runSettings defaultSettings{settingsTimeout = 20, settingsPort= porti} waiMessageFlow

-- | Exactly the same as runNavigation, but with TLS added.
-- Expects certificate.pem and key.pem in project directory.

--runSecureNavigation = runSecureNavigation' TLS.defaultTlsSettings defaultSettings
--
--runSecureNavigation' :: TLSSettings -> Settings -> String -> FlowM Html (Workflow IO) () -> IO ()
--runSecureNavigation' t s n f = do
--    unless (null n) $ setNoScript n
--    addMessageFlows[(n, runFlow f)]
--    porti <- getPort
--    let s' = setPort porti s
--    wait $ TLS.runTLS t s' waiMessageFlow
