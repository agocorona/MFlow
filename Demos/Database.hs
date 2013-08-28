{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Database where
import MFlow.Wai.Blaze.Html.All hiding (select)

import Data.Typeable
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.Monoid



import Data.String
import  Aws
import  Aws.SimpleDb hiding (select)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Lazy(toChunks,fromChunks)
import Network

import Debug.Trace
(!>)= flip trace

data  MyData= MyData{idnumber :: Int, textdata :: T.Text} deriving (Typeable, Read, Show)  -- that is enough for file persistence
instance Indexable MyData where
   key=  show . idnumber    -- just to notify what is the key of the register
   defPath = const ""

data Options= NewText | ListTexts deriving (Show, Typeable)


-- to run it alone, change askm by ask, remove Menu.hs and uncomment this:
--main=   do
--  setAmazonSimpleDB
--  syncWrite  $ Asyncronous 1 defaultCheck  1000
--  index idnumber
--  runNavigation "" $ transientNav database

     
database= do
     r <- askm $   p << "menu"
               ++> wlink NewText   << p << "enter a new text"
               <|> wlink ListTexts << p <<  "All texts"


     case r of
         NewText -> do
         
              text <- askm $ p << "insert the text" ++> getMultilineText "" <++ br
                           <** submitButton "enter"

              n <- allTexts >>= return . length

              liftIO . atomically . newDBRef $ MyData n text  -- store the name in the cache (later will be written to disk automatically)
              listtests

         ListTexts -> listtests
     where
     listtests= do
              -- query for all the names stored in all the registers
              all <- allTexts
              askm $    h3 << "list of all texts"
                   ++> mconcat[p <<  t | t <- all]
                   ++> wlink () << p << "click here to go to the  menu"
                   <++ b << "or the back button for a new database action"

     allTexts= liftIO . atomically $ select textdata $ idnumber .>=. (0 :: Int)

sdbCfg =  defServiceConfig

--
domain = fromString "mflowdemo"



setAmazonSimpleDB= withSocketsDo $ do
 cfg <- baseConfiguration
 simpleAws cfg sdbCfg $ deleteDomain domain
 simpleAws cfg sdbCfg $ createDomain domain
 setDefaultPersist $ Persist{
   readByKey= \key -> withSocketsDo $do
       r <- simpleAws cfg sdbCfg $ getAttributes (T.pack key) domain
       case r of
        GetAttributesResponse [ForAttribute _ text] -> return $ Just   $ fromChunks [encodeUtf8 text]
        _ -> return Nothing,

   write= \key str -> withSocketsDo $ do
       simpleAws cfg sdbCfg
                     $ putAttributes  (T.pack key)  [ForAttribute tdata (SetAttribute (T.concat $ map decodeUtf8 $ toChunks str) True)] domain
       return (),
   delete= \key    ->withSocketsDo $ do
     simpleAws cfg sdbCfg $ deleteAttributes (T.pack key)  [ForAttribute tdata DeleteAttribute] domain
     return ()
     }

tdata= fromString "textdata"
