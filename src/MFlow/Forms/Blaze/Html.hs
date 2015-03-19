{-# OPTIONS  -XOverloadedStrings -XFlexibleInstances -XTypeSynonymInstances
           #-}
{- |
Instantiation of the 'FormInput' class for blaze-html <http://hackage.haskell.org/package/blaze-html>

This package is included in "MFlow.Wai.Blaze.Hml.All".

Use it to create applications with this kind of formatting.
-}
module MFlow.Forms.Blaze.Html where
import MFlow
import MFlow.Forms
import MFlow.Cookies(contentHtml)
import Data.ByteString.Lazy.UTF8
import qualified Data.String as S
import Text.Blaze.Html
import qualified Text.Blaze.Internal as I
import Text.Blaze.Html5 as St
import Text.Blaze.Html5.Attributes as At
import Text.Blaze.Html.Renderer.Utf8 -- (renderHtml)
import Control.Monad.Trans
import Data.Typeable
import Data.Monoid
--import Data.Text.Encoding
--import Data.Text as T
import Text.Blaze.Internal

-- | Used to insert html elements within a tag with the appropriate infix priority for the
-- other operators used in MFlow. Also it can be used for adding markup to
-- widgets with this signature such are 'wlink' ad 'setOption'
(<<) :: ToMarkup a => (Markup -> t) -> a -> t
(<<) tag v= tag $ toMarkup v

infixr 7 <<

--fromUtf8 = toValue . encodeUtf8 . T.pack


instance FormInput Html where
    toByteString  =  renderHtml
    toHttpData = HttpData [contentHtml] [] . toByteString
    ftag x=  I.Parent (S.fromString x) (S.fromString $ "<" ++ x) (S.fromString $ "</"++ x ++">")
              -- (mempty :: I.MarkupM () )

    inred =  b ! At.style  "color:red"

    finput n t v f c=
       let
        tag= input ! type_ (toValue t) ! name  (toValue n) !value (toValue v)
        tag1= if f then tag  ! checked (toValue ("" ::String)) else tag
       in case c of Just s -> tag1 ! onclick  (toValue s) ; _ -> tag1

    ftextarea nam text=  textarea ! name  (toValue nam) <<  text

    fselect nam list = select ! name  (toValue nam) << list
    foption  name v msel=
      let tag=  option ! value  (toValue name)  <<  v
      in if msel then tag ! selected (toValue ("" ::String)) else tag


    formAction action method1 form = St.form !  acceptCharset "UTF-8" ! At.action  (toValue action) ! method  (toValue method1) $ form

    fromStr= toMarkup
    fromStrNoEncode  = preEscapedToMarkup
    flink  v str = a ! href  (toValue  v) << str

    attrs tag  [] = tag
    attrs tag ((n,v):attribs) =
       let tag'= tag ! (customAttribute $ stringTag n) (toValue v)
       in attrs tag' attribs
