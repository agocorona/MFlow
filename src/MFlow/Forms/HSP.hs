{-# OPTIONS -F -pgmFtrhsx  -XTypeFamilies  -XOverloadedStrings -XUndecidableInstances -XOverlappingInstances -XTypeSynonymInstances -XFlexibleInstances #-}

{- |
Instantiation of the 'FormInput' class for the HSP package <http://hackage.haskell.org/package/hsp>
 for embedding widgets within HTML-XML formatting

-}

module MFlow.Forms.HSP
 where

import MFlow
import MFlow.Cookies(contentHtml)
import MFlow.Forms
import Control.Monad.Trans
import Data.Typeable
import HSP.Monad
import HSP.XML
import HSP.XMLGenerator
import Data.Monoid
import Control.Monad(when)
import Data.ByteString.Lazy.Char8(unpack,pack)
import System.IO.Unsafe
import Data.TCache.Memoization (Executable (..))
import Data.Text.Lazy.Encoding
import Data.String



instance (XMLGen m,XML ~ XMLType m, EmbedAsChild m(XMLType m)) => Monoid (XMLGenT m XML) where
    mempty =   <span/>
    mappend  x  y= <span> <% x %> <% y %> </span>
    mconcat xs=  <span> <% [<% x %> |  x <- xs] %> </span>

instance Typeable  (XMLGenT m XML) where
    typeOf= \_ ->  mkTyConApp(mkTyCon3 "hsp" "HSP.XMLGenerator" "XMLGenT m XML") []

instance (XMLGen m,XML ~ XMLType m
         ,EmbedAsChild m XML
         ,EmbedAsAttr m (Attr  String String)
         ,Executable m
         ,SetAttr m XML)
         => FormInput (XMLGenT m XML)   where
    toByteString =  encodeUtf8 . renderXML . execute . unXMLGenT
    toHttpData = HttpData [contentHtml ] [] . toByteString
    ftag t =  \e -> genElement (toName t) [] [asChild e]

    fromStr s =   <span><% s %></span>
    fromStrNoEncode s= <pcdata> pcdataToChild s </pcdata>
    finput typ name value checked onclick=
      <input type= (typ)
             name= (name)
             value=(value)
             checked=(show checked)
             onclick=(case onclick of Just s -> s ; _ -> "")/>

    ftextarea  name text= <textarea name=(name) > <% text %> </textarea>

    fselect name list=  <select name=(name)> <%list%> </select>
    foption  n v msel=
                  <option value=(n) selected=(selected msel ) >
                      <% v %>
                  </option>

          where
          selected msel = if msel then "true" else  "false" :: String

    flink  v str = <a href=(v)> <% str %> </a>

    inred x= <b style= "color:red"> <% x %> </b>

    formAction action form = <form action=(action) method="post" > <% form %> </form>


    attrs tag  attrs=  tag <<@ map (\(n,v)-> n:=v) attrs
