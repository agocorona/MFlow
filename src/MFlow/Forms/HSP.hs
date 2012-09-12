{-# OPTIONS -F -pgmFtrhsx   -XUndecidableInstances -XOverlappingInstances -XTypeSynonymInstances -XFlexibleInstances #-}

{- | Instantiation of "MFlow.Forms" for the hsp package
it includes additional features for embedding widgets within HTML formatting

-}

module MFlow.Forms.HSP
 where


import MFlow.Forms
import Control.Monad.Trans
import Data.Typeable
import HSP
import Data.Monoid
import Control.Monad(when)
import Data.ByteString.Lazy.Char8(unpack)



instance Monoid (HSP XML) where
    mempty =   <span/>
    mappend x y= <span> <% x %> <% y %> </span>
    mconcat xs= <span> <% [<% x %> | x <- xs] %> </span>

instance FormInput (HSP XML)   where

    fromString s =   <span><% s %></span>


    finput typ name value checked onclick=
      <input type= (typ)
             name=(name)
             value=(value)
             checked=(checked)
             onclick=(case onclick of Just s -> s ; _ -> "")/>

    ftextarea  name text= <textarea name=(name) > <% text %> </textarea>


    foption  name list msel=
          <select name=(name)>
            <% map (\(n,v) ->
                  <option value=(n) selected=(selected msel n) >
                      <% v %>
                  </option> )
                  list
            %>
          </select>
          where
          selected msel n= if Just n == msel then "true" else  "false"

    flink  v str = <a href=(v)> <% str %> </a>

    inred x= <b style= "color:red"> <% x %> </b>

    formAction action form = <form action=(action) method="post" > <% form %> </form>


    addAttributes tag  attrs=  tag <<@ map (\(n,v)-> n:=v) attrs
