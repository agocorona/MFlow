{-# LANGUAGE DeriveDataTypeable  #-}
module Database where


import MFlow.Wai.Blaze.Html.All hiding (select)
import Menu
import Data.Typeable
import Data.TCache.IndexQuery
import Data.TCache.DefaultPersistence
import Data.Monoid

data  MyData= MyData{idnumber :: Int, textdata :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence
instance Indexable MyData where  key=  show . idnumber    -- just to notify what is the key of the register

data Options= NewText | ListTexts deriving (Show, Typeable)


-- to run it alone, change askm by ask and uncomment this:
--main= do
--  index name
--  runNavigation "" $ transientNav database

     
database= do
     r <- askm $   do
              p << "menu"
               ++> wlink NewText   << p << "enter a new text"
               <|> wlink ListTexts << p <<  "All texts"


     case r of
         NewText -> do
              text <- askm $ p << "insert the text" ++> getMultilineText "" <++ br
                           <** submitButton "enter"
              n <- allTexts >>= return . length
              liftIO . atomically . newDBRef $ MyData n text   -- store the name in the cache (later will be written to disk automatically)
              return()

         ListTexts -> do
              -- query for all the names stored in all the registers
              all <- allTexts

              askm $   h3 << "list of all texts"
                   ++> mconcat[p <<  t | t <- all]
                   ++> wlink () << p << "click here to go to the database menu"


     database
     where
     allTexts= liftIO . atomically $ select textdata $ idnumber .>=. (0 :: Int)

     


