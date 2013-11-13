{-# LANGUAGE DeriveDataTypeable, RecordWildCards
           , OverloadedStrings, StandaloneDeriving
           , ScopedTypeVariables, CPP #-}
module Database where

import Data.Typeable
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.TCache.Memoization
import Data.Monoid

import Data.String
import Aws
import Aws.SimpleDb hiding (select)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy.Char8(toChunks,fromChunks,pack,unpack)
import Network

import Aws.S3
import Data.Conduit
import Network.HTTP.Conduit
import qualified Data.Conduit.List as CList
import Data.List as L hiding (delete)
import Data.Maybe
import System.IO.Unsafe
import Control.Exception

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(select, page)
import Menu
#endif



-- to run it alone,  remove Menu.hs and uncomment this:

--askm= ask
--
--main= do
----  setIndexPersist $ amazonSDBPersist cfg "testmflowdemo"
----  setAmazonSDBPersist "testmflowdemo"
--  syncWrite  $ Asyncronous 120 defaultCheck  1000
--  index idnumber
--  runNavigation "" $ transientNav database

data  MyData= MyData{idnumber :: Int, textdata :: T.Text} deriving (Typeable, Read, Show)  -- that is enough for file persistence
instance Indexable MyData where
   key=  show . idnumber    -- the key of the register


domain= "mflowdemo"

instance  Serializable MyData where
  serialize=  pack . show
  deserialize=   read . unpack
  setPersist = const . Just . unsafePerformIO $ withSocketsDo $ do
     cfg <- baseConfiguration
     simpleAws cfg sdbCfg $ deleteDomain domain   -- delete the domain to start afresh
     simpleAws cfg sdbCfg $ createDomain domain
     return $ amazonSDBPersist cfg domain

 
data Options= NewText | Exit deriving (Show, Typeable)


     
database= do
     all <- allTexts
     r <- page $ listtexts all

     case r of
         NewText -> do
              text <- page $   p "Insert the text"
                           ++> htmlEdit ["bold","italic"] ""  -- rich text editor with bold and italic buttons
                                        (getMultilineText "" <! [("rows","3"),("cols","80")]) <++ br
                           <** submitButton "enter"

              liftIO . atomically . newDBRef $ MyData (length all) text  -- store the name in the cache (later will be written to disk automatically)
              database 

         Exit -> return ()
     where
     menu=   wlink NewText   << p "enter a new text" <|>
             wlink Exit      << p "exit to the home page"

     listtexts all  =  do
           h3 "list of all texts"
           ++> p "the texts are deleted on every restart by heroku"
           ++> mconcat[p $ preEscapedToHtml t >> hr | t <- all]
           ++> menu
           <++ b "or press the back button or enter the  URL any other page in the web site"


     allTexts= liftIO . atomically . select textdata $ idnumber .>=. (0 :: Int)




sdbCfg =  defServiceConfig
s3cfg = Aws.defServiceConfig :: S3Configuration Aws.NormalQuery



setAmazonSDBPersist domain = withSocketsDo $ do
 cfg <- baseConfiguration
-- simpleAws cfg sdbCfg $ deleteDomain domain
 simpleAws cfg sdbCfg $ createDomain domain
 setDefaultPersist $ amazonSDBPersist cfg domain

amazonSDBPersist cfg domain = Persist{
   readByKey= \key -> withSocketsDo $ do
       r <- simpleAws cfg sdbCfg $ getAttributes (T.pack key) domain
       case r of
        GetAttributesResponse [ForAttribute _ text] -> return $ Just   $ fromChunks [encodeUtf8 text]
        _ -> return Nothing,

   write= \key str -> withSocketsDo $ do
       simpleAws cfg sdbCfg
                     $ putAttributes  (T.pack key)  [ForAttribute tdata (SetAttribute (T.concat $ map decodeUtf8 $ toChunks str) True)] domain
       return (),
   delete= \ key  -> withSocketsDo $ do
     simpleAws cfg sdbCfg $ deleteAttributes (T.pack key)  [ForAttribute tdata DeleteAttribute] domain
     return ()
     }

tdata=  "textdata"

deriving instance Show GetObjectResponse

instance Show (ResumableSource a b) where show _= "source"

setAmazonS3Persist bucket = withSocketsDo $ do
  cfg <- baseConfiguration
  setDefaultPersist $ amazonS3Persist cfg  bucket

amazonS3Persist cfg  bucket= Persist{
   readByKey = \key -> (withSocketsDo $ withManager $ \mgr -> do
     mr <- do
               o@(GetObjectResponse hdr rsp) <- 
                          Aws.pureAws cfg s3cfg mgr
                            $ getObject
                              bucket
                              (fromString key)  -- !> key
               if omDeleteMarker hdr
                then return Nothing  -- !> "nothing"
                else fmap Just $ responseBody rsp $$+- CList.consume  -- !> "just"
     return $ fmap fromChunks mr)
    `Control.Exception.catch` (\(e :: SomeException) -> return Nothing),
   write = \key str -> do
        withSocketsDo $ withManager $ \mgr -> do

          Aws.pureAws cfg s3cfg mgr
            $ putObject
              bucket
              (fromString key)
              (RequestBodyLBS str)
          return(),

   delete = \key -> withSocketsDo $ withManager $ \mgr -> do
          Aws.pureAws cfg s3cfg mgr
            $ DeleteObject (fromString key) bucket
          return()


     }
