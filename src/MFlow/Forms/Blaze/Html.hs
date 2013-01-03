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
import MFlow
import MFlow.Forms
import MFlow.Cookies(contentHtml)
import Data.ByteString.Lazy.Char8

import Text.Blaze.Html
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html5 as St
import Text.Blaze.Html5.Attributes as At
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad.Trans
import Data.Typeable
import Data.String
import Data.Monoid
import Unsafe.Coerce

import Debug.Trace
(!>)= flip trace




(<<) tag v= tag $ toMarkup v

infixr 7 <<

instance FormInput Html where
    toByteString  =  renderHtml
    toHttpData = HttpData [contentHtml ] [] . toByteString
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
    fromStrNoEncode  = preEscapedToMarkup
    flink  v str = a ! href  (fromString  v) << str

    attrs tag  [] = tag
    attrs tag ((n,v):attribs) =
       let tag'= tag ! (customAttribute $ stringTag n) (fromString v)
       in attrs tag' attribs







