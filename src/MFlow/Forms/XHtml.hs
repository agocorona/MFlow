-----------------------------------------------------------------------------
--
-- Module      :  Control.MessageFlow.Forms.XHtml
-- Copyright   :  Alberto Gónez Corona
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
--
-- |
--
-----------------------------------------------------------------------------
{- Instantiation of "MFlow.Forms" for the xhtml package "Text.XHtml" module
it includes additional XHtml style operators for embedding widgets within XHtml formatting

-}

{-# OPTIONS -XMultiParamTypeClasses
            -XFlexibleInstances
            -XUndecidableInstances
            -XTypeSynonymInstances
            -XFlexibleContexts
            -XTypeOperators
            #-}


module MFlow.Forms.XHtml where


import MFlow.Forms
import Text.XHtml as X
import Control.Monad.Trans
import Data.Typeable

instance Monad m => ADDATTRS (View Html m a) where
  widget ! atrs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  [head fs ! atrs] mx


instance FormInput  Html  where

    inred = X.bold ![X.thestyle "color:red"]
    finput n t v f c= X.input ! ([thetype t ,name n, value v] ++ if f then [checked]  else []
                              ++ case c of Just s ->[strAttr "onclick" s]; _ -> [] )
    ftextarea name text= X.textarea ! [X.name name] << text

    foption name list msel=  select ![ X.name name] << (concatHtml
            $ map (\(n,v) -> X.option ! ([value n] ++ selected msel n) << v ) list)

            where
            selected msel n= if Just n == msel then [X.selected] else []

    addAttributes tag attrs = tag ! (map (\(n,v) -> strAttr n v) attrs)


    formAction action form = X.form ! [X.action action, method "post"] << form
    fromString = stringToHtml


    flink  v str = toHtml $ hotlink  v << str


