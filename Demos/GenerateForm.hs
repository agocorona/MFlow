-----------------------------------------------------------------------------
--
-- Module      :  GenerateForm
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
{-# LANGUAGE DeriveDataTypeable #-}
module GenerateForm (

) where
import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Internals
import Control.Monad.State
import Unsafe.Coerce
import Data.Typeable
import Data.Monoid
import Data.String

import Debug.Trace

(!>)= flip trace

main=runNavigation "nav" $ step $ do
    desc <-  ask $ createForm [] "form"
    ask$  p << show desc ++> noWidget


createForm desc title'= (do
   wdesc <- chooseWidget <++ hr
   let title= title' ++".html"
   content <- liftIO $ readtField  (b << "HOHAHAHAHAHAHAH") title
   fieldview <- generate  wdesc
   liftIO . writetField title $ content <> fieldview
   let ndesc= desc++ [wdesc]
   edTemplate "editor" title $ return ()
   return ndesc
  <+> wlink "save" << p << "save")
 `wcallback` \r ->   case r of
   (Just desc', _) -> createForm desc' title'
   (_, Just _) ->     return desc


newtype Seq= Seq Int deriving (Typeable)

generate desc= View $ do
    Seq n <- getSessionData `onNothing` return (Seq 0)
    s <- get
    put s{mfSequence= n}
    FormElm render _ <-  case desc of
        Stringv -> runView $ getString Nothing >> return ()
        Intv ->   runView $ getInt Nothing >> return()
        TextArea -> runView $  getMultilineText  (fromString "") >> return ()
        OptionBox xs -> runView $ do
                        getSelect (setSelectedOption ""
                           (p  << "select a option") <|>
                           firstOf[setOption op  (b <<  op) | op <- xs])
                        return ()


    put s

    return $ FormElm [] $ Just ( mconcat render :: Html)

data WType= Intv | Stringv | TextArea |OptionBox[String] | Combo [String] deriving (Typeable,Read,Show)

chooseWidget=
    ul <<<(li <<< do
            wlink () << "text field"
            ul <<<(li <<< wlink Intv << "returning Int"
               <|> li <<< wlink Stringv << "returning string")
       <|> li <<< wlink TextArea << "text area"
       <|> li <<< do
              wlink  "options" << "options"
              ul <<< do
                 ops <- getOptions []
                 return $ OptionBox ops

       <|> li <<< do
              wlink "combo" << "conboBox"
              ul <<< do
                 ops <- getOptions []
                 return $ Combo ops)


getOptions ops= do
    op <- getString Nothing <++ p << " or"
          <|> wlink "exit" << p << " exit"
    if op == "exit"
     then return ops
     else getOptions $ op:ops

