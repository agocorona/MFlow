-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Forms.Blaze.Html
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS  -XOverloadedStrings -XFlexibleInstances -XTypeSynonymInstances
           #-}
module MFlow.Forms.Blaze.Html where

import MFlow.Forms
import Data.ByteString.Lazy.Char8

import Text.Blaze.Html
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html4.Strict as St
import Text.Blaze.Html4.Strict.Attributes as At
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad.Trans
import Data.Typeable
import Data.String
import Data.Monoid
import Unsafe.Coerce

instance ToByteString Html where
  toByteString  =  renderHtml

(<<) tag v= tag $ toMarkup v

instance FormInput Html where
    ftag x=  I.Parent (fromString x) (fromString $ "<"++x) (fromString $ "</"++ x ++">")
              -- (mempty :: I.MarkupM () )

    inred =  b ! At.style (fromString "color:red")

    finput n t v f c=
       let
        tag= input ! type_ (fromString t) ! name  (fromString n) !value  (fromString v)
        tag1= if f then tag  ! checked (fromString "") else tag
       in case c of Just s -> tag1 ! onclick  (fromString s) ; _ -> tag1

    ftextarea nam text=  textarea ! name  (fromString nam) <<  text

    fselect nam list = select ! name  (fromString nam) << list
    foption  name v msel=
      let tag=  option ! value  (fromString name)  <<  v
      in if msel then tag ! selected (fromString "") else tag


    formAction action form = St.form ! At.action  (fromString action) ! method  (fromString "post") $ form

    fromStr= toMarkup
    fromStrNoEncode= preEscapedToMarkup

    flink  v str = a ! href  (fromString  v) << str

    addAttributes tag  [] = tag
    addAttributes tag ((n,v):attribs) =
       let tag'= tag ! (customAttribute $ stringTag n) (fromString v)
       in addAttributes tag' attribs







