-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Forms.Load
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

{-# OPTIONS -XMultiParamTypeClasses
            -XFlexibleInstances
            -XUndecidableInstances
            -XTypeSynonymInstances
            -XFlexibleContexts
            -XTypeOperators
            #-}
module MFlow.Forms.Load  where

import MFlow.Forms
import Control.Monad.Trans
import Data.Typeable
{-
como detectar un numero
reejecutar un flow a partir de un wf
stepdebug
 graba las request en vez del reszultado
 step
-}
data LElem view=None view | ShrtString String String view | LongString String view

type Load view= [LElem view]

instance FormInput  Html  where

    inred x= None x
    finput n t v f c= ShrtString n t $ finput n t v f c
    ftextarea name text= LongString name $ ftextarea name text

    foption name list msel=  ShrtString name "option" $ foption name list msel

    addAttributes tag attrs = None $ addAttributes tag attrs

    formAction action form = None form
    fromString s= None $ formString s


    flink v str = ShrtString v "link" $ flink  v str



