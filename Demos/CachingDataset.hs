{-# LANGUAGE CPP, DeriveDataTypeable #-}
module CachingDataset where
import Data.Typeable
import Control.Monad
import Data.Time

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav cachingDataset
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif
newtype NextReg = NextReg Int deriving Typeable


cachingDataset= do
   page $ iterateResults [1..] 5  **> wlink () << b << "exit"
   where
   iterateResults allnames n= pageFlow "d" $ witerate $  do
      public
      maxAge 200
      time <- liftIO getCurrentTime
      b << "Read from the server at: " ++> dField(wraw . fromStr $ show time) <++ br
      NextReg ind <- getSessionData `onNothing` return (NextReg 0)
      foldl (**>) noWidget [dField(getData i ) <++ br | i <- [ind .. ind + n - 1]]
      r <-     dField (wlink (next ind ) << b << "next" ) <++ fromStr " "
           <|> dField (wlink (prev ind ) << b << "prev")
           <|> restp
      setSessionData $ NextReg r
      where

      getData i =  wraw . fromStr . show $ allnames !! i

      next i  =  i + n
      prev i  = case i >= n of True ->  i - n; _ -> i
