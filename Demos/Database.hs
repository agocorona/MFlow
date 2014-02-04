{-# LANGUAGE DeriveDataTypeable, RecordWildCards
           , OverloadedStrings, StandaloneDeriving
           , ScopedTypeVariables, CPP #-}
module Database where

import Data.Typeable
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.TCache.AWS
import Data.Monoid
import qualified Data.Text as T
import Data.String
import Data.ByteString.Lazy.Char8 hiding (index)


-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= do
  syncWrite  $ Asyncronous 120 defaultCheck  1000
  index idnumber
  runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(select, page)
import Menu
#endif



-- to run it alone,  remove Menu.hs and uncomment this:

--askm= ask
--
--main= do
--  syncWrite  $ Asyncronous 120 defaultCheck  1000
--  index idnumber
--  runNavigation "" $ step database

data  MyData= MyData{idnumber :: Int, textdata :: T.Text} deriving (Typeable, Read, Show)  -- that is enough for file persistence
instance Indexable MyData where
   key=  show . idnumber    -- the key of the register


domain= "mflowdemo"

instance  Serializable MyData where
  serialize=  pack . show
  deserialize=  read . unpack
  setPersist =  const . Just $ amazonS3Persist domain -- False
 
data Options= NewText | Exit deriving (Show, Typeable)


     
database= do
     liftIO $ index idnumber
     database'


database'= do
     all <- allTexts

     r <- page $ listtexts all

     case r of
         NewText -> do
              text <- page $   p "Insert the text"
                           ++> htmlEdit ["bold","italic"] ""  -- rich text editor with bold and italic buttons
                                        (getMultilineText "" <! [("rows","3"),("cols","80")]) <++ br
                           <** submitButton "enter"

              addtext all text  -- store the name in the cache (later will be written to disk automatically)
              database' 

         Exit -> return ()
     where
     menu= wlink NewText   << p "enter a new text" <|>
           wlink Exit      << p "exit to the home page"

     listtexts all  =  do
           h3 "list of all texts"
           ++> mconcat[p $ preEscapedToHtml t >> hr | t <- all]
           ++> menu
           <++ b "or press the back button or enter the  URL any other page in the web site"

     addtext all text= liftIO . atomically . newDBRef $ MyData (Prelude.length all) text
     allTexts= liftIO . atomically . select textdata $ idnumber .>=. (0 :: Int)

