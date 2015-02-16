-----------------------------------------------------------------------------
--
-- Module      :  Control.MessageFlow.Forms.XHtml
-- Copyright   :  Alberto Gónez Corona
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{- | Instances of `FormInput`  for  the 'Text.XHtml' module of the xhtml package
-}

{-# OPTIONS -XMultiParamTypeClasses
            -XFlexibleInstances
            -XUndecidableInstances
            -XTypeSynonymInstances
            -XFlexibleContexts
            -XTypeOperators
            #-}


module MFlow.Forms.XHtml where

import MFlow (HttpData(..))
import MFlow.Forms
import MFlow.Cookies(contentHtml)
import Data.ByteString.Lazy.Char8(pack,unpack)
import qualified Data.Text as T
import Text.XHtml.Strict as X
import Control.Monad.Trans
import Data.Typeable

instance Monad m => ADDATTRS (View Html m a) where
  widget ! atrs= widget `wmodify`  \fs mx -> return ((head fs ! atrs:tail fs), mx)



instance FormInput  Html  where
    toByteString  =  pack. showHtmlFragment
    toHttpData = HttpData [contentHtml] []  . toByteString
    ftag t= tag t
    inred = X.bold ![X.thestyle "color:red"]
    finput n t v f c= X.input ! ([thetype t ,name  n, value  v] ++ if f then [checked]  else []
                              ++ case c of Just s ->[strAttr "onclick"  s]; _ -> [] )
    ftextarea name text= X.textarea ! [X.name  name] <<  T.unpack text

    fselect name list = select ![ X.name  name] << list
    foption  name v msel=  X.option ! ([value  name] ++ selected msel) <<  v
            where
            selected msel = if  msel then [X.selected] else []

    attrs tag attrs = tag ! (map (\(n,v) -> strAttr  n  v)  attrs)



    formAction action form = X.form ! [X.action  action, method "post"] << form
    fromStr = stringToHtml
    fromStrNoEncode= primHtml

    flink  v str = toHtml $ hotlink  (  v) << str

instance Typeable Html where
     typeOf =  \_ -> mkTyConApp (mkTyCon3 "xhtml" "Text.XHtml.Strict" "Html") []
