{-#LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module TryHplay(tryHplay) where

import MFlow.Wai.Blaze.Html.All
import Haste.Compiler
import Data.Default
import Prelude hiding (id,div,head)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String
import Data.TCache.DefaultPersistence
import Data.TCache.IndexText

import System.Directory
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Typeable
import Data.Monoid
import Text.Blaze.Html5.Attributes as At hiding (step,name)
import qualified Data.Text.Lazy as TL

projects= "./Demos/"

data Examples= Examples [String] deriving (Read,Show,Typeable)
instance Indexable Examples where key = const "examples"
instance Serializable Examples where
  serialize = B.pack . show
  deserialize =  read . B.unpack

listExamples (Examples list)= list

tryHplay= do
    indexList listExamples (map TL.pack )
    examples <- atomically $ newDBRef $ Examples ["example.hs"]

--    setFilesPath projects


    let trynumber= 3

    Examples exampleList <- liftIO $ atomically $ readDBRef examples
                         `onNothing` error "examples empty"
    (js,hs) <- page $   pageFlow "input" $ do


          example <- b  "you can load also one of these examples "
                     ++> firstOf[wlink e << e <++ " " | e <- exampleList]
                     <|> return "none"

          extext <- if example /= "none" then liftIO $ TIO.readFile $ projects ++ example else return ""

          r <- p <<< (getMultilineText extext <! [("style","width:100%;height:300")]
                        <++ br
                        <** submitButton "send"
                        <++ br)
          let haskell=  T.unpack r
          r <- p <<< do liftIO $ compile def "./" $ InString haskell

          out <- case r of
              Failure errs -> fromStr errs ++> empty
              Success (OutString out) -> return out

--          p <<< submitButton  "execute"
          let jsfile = show trynumber ++ ".js"
          liftIO $ writeFile  (projects ++ jsfile) out
          return (jsfile,haskell)

    setHeader $ \w ->  docTypeHtml $ do
           head $ script ! type_ "text/javascript" ! src (fromString $ "/"++ js) $ fromStr ""
           body $ do
             div ! At.style "background:gray" ! id "idelem" $ fromStr ""
             w

    page $ wform $
               (getString Nothing <! [("placeholder","give a program name to save")])

                `validate` (\name -> do

                  list <- liftIO $ atomically $ listExamples `containsElem`  name
                  if null list
                       then liftIO $ do
                           writeFile  (projects ++name) hs
                           renameFile (projects ++js) $ projects ++ name++ ".js"
                           atomically $ writeDBRef examples $ Examples $ name:exampleList
                           return Nothing
                       else return $ Just "name already used")

               **> submitButton "send" **> return  ()
