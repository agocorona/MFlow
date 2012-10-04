{-# OPTIONS -XScopedTypeVariables   #-}
module Fay.Event where

--import Language.Fay.JQuery
--import Language.Fay.Prelude
import Language.Fay
import Language.Fay.Compiler
import Language.Fay.Types
import Language.Fay.FFI
import LanguageFayJQuery
import LanguageFayDOM
import Data.List
import Data.Char
import MFlow.Wai.XHtml.All hiding(select)
import Data.Default
import System.IO.Unsafe
import Control.Monad


instance Default CompileConfig where
  def = CompileConfig False False False True [] False False [] False



compFile file= unsafePerformIO $ do
    s <-  readFile file
    compM s

comp proc = unsafePerformIO . compM $ getExpr proc

compM source= do
   r <-  compileViaStr def compileExp source
   case r of
    Left err    -> error $ show err
    Right (s,_) -> return s

thisfile= "Fay.Event.hs"
thisfilestr = unsafePerformIO $ readFile thisfile

getExpr n=
  let s=   accept [] untilOtherFunc  $ reject (isPrefixOf ('\n':n)) thisfilestr
  in tail $ dropWhile (/='=') $ drop(length n) s

reject exp xs= case exp xs of
    True -> tail xs
    False -> reject exp $ tail xs

accept rs exp xs= case exp xs of
    False -> accept (head xs:rs) exp (tail xs)
    True -> reverse $ tail rs

untilOtherFunc (x:y:xs)= x=='\n' && not (isSpace y)



headerf html =
   thehtml << (header
                 << (script ![thetype "text/javascript", strAttr "src" (linkFile "jquery.js")] << noHtml
                 +++ script ![thetype "text/javascript", strAttr "src" (linkFile "LanguageFayJQuery.js")] << noHtml
                 +++ script ![thetype "text/javascript", strAttr "src" (linkFile "LanguageFayPrelude.js")] << noHtml
                 +++ script ![thetype "text/javascript"] << (primHtml "function fai(){alert('fai')}")))
           +++ tbody << html

main2= print $ comp "fai"

main :: IO ()
main = do
    addFileServerWF
    addMessageFlows[("", transient $ runFlow fl)]
    run 80 waiMessageFlow

aler= alert "hola"

fai=  alert "hi"

--    this <- getThis
--    text <- getText this
--    select "<li></li>" >>= appendTo this >>= setText "hello"

--    setHtml "" di
--    forM ["hello","whatsap"] $ \u -> do

fl :: FlowM Html IO ()
fl= do
   setHeader headerf
   ask $  wlink () (bold << "refresh")
       <++ thediv ! [strAttr "id" "autocomp", strAttr "onclick" "alert('fai')"] << "asdasadhi"
   fl


