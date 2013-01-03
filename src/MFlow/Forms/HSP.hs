{-# OPTIONS -F -pgmFtrhsx   -XUndecidableInstances -XOverlappingInstances -XTypeSynonymInstances -XFlexibleInstances #-}

{- | Instantiation of "MFlow.Forms" for the hsp package
it includes additional features for embedding widgets within HTML-XML formatting

-}

module MFlow.Forms.HSP
 where

import MFlow
import MFlow.Cookies(contentHtml)
import MFlow.Forms
import Control.Monad.Trans
import Data.Typeable
import HSP
import Data.Monoid
import Control.Monad(when)
import Data.ByteString.Lazy.Char8(unpack,pack)
import System.IO.Unsafe



instance Monoid (HSP XML) where
    mempty =   <span/>
    mappend x y= <span> <% x %> <% y %> </span>
    mconcat xs= <span> <% [<% x %> | x <- xs] %> </span>

instance Typeable (HSP XML) where
   typeOf= \_ ->  mkTyConApp(mkTyCon "HSP XML") []

instance FormInput (HSP XML)   where
    toByteString x= unsafePerformIO $ do
       (_,r) <-  evalHSP Nothing x
       return .  pack $ renderXML r
    toHttpData = HttpData [contentHtml ] [] . toByteString
    ftag t =  \e -> genElement (toName t) [] [asChild e]

    fromStr s =   <span><% s %></span>
    fromStrNoEncode s= <pcdata> pcdataToChild s </pcdata>
    finput typ name value checked onclick=
      <input type= (typ)
             name=(name)
             value=(value)
             checked=(checked)
             onclick=(case onclick of Just s -> s ; _ -> "")/>

    ftextarea  name text= <textarea name=(name) > <% text %> </textarea>

    fselect name list=  <select name=(name)> <%list%> </select>
    foption  n v msel=
                  <option value=(n) selected=(selected msel ) >
                      <% v %>
                  </option>

          where
          selected msel = if msel then "true" else  "false"

    flink  v str = <a href=(v)> <% str %> </a>

    inred x= <b style= "color:red"> <% x %> </b>

    formAction action form = <form action=(action) method="post" > <% form %> </form>


    attrs tag  attrs=  tag <<@ map (\(n,v)-> n:=v) attrs
