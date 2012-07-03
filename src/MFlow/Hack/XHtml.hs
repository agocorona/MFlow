-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Hack.XHtml
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

{-
Instantiations necessary for "MFlow.Hack" to use "Text.XHtml" as format for generating output
-}

{-# OPTIONS -XMultiParamTypeClasses #-}

module MFlow.Hack.XHtml (

) where
import MFlow
import Hack
import MFlow.Hack.Response
import Text.XHtml
import Data.Typeable
import Data.ByteString.Lazy.Char8 as B(pack,unpack, length, ByteString)

instance ToResponse Html where
  toResponse x= Response{ status=200, headers=[]
                        , Hack.body= pack $ showHtml x}

instance Typeable Html where
     typeOf =  \_ -> mkTyConApp (mkTyCon "Text.XHtml.Strict.Html") []
--
--instance ConvertTo Html TResp where
--     convert = TResp

