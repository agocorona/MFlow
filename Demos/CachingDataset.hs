{-# LANGUAGE DeriveDataTypeable #-}
module CachingDataset where
import MFlow.Wai.Blaze.Html.All
import Data.Typeable
import Control.Monad
import Data.Time
import Debug.Trace
(!>)= flip trace

newtype NextReg = NextReg Int deriving Typeable

main= runNavigation "showResults" $ transientNav $ cachingDataset

cachingDataset=
   page $ iterateResults   [1..100] 5 

   where
   iterateResults allnames n = witerate $ do
      let len= length allnames
      public
      maxAge 200
      time <- liftIO getCurrentTime
      b << "Read from the server at: " ++> dField(wraw . fromStr $ show time) <++ br
      NextReg ind <- getSessionData `onNothing` return (NextReg 0)
      foldl (**>) noWidget [dField(getData i len) <++ br | i <- [ind..ind+n-1]]
      r <-     dField (wlink (next ind len) << b << "next" ) <++ fromStr " "
           <|> dField (wlink (prev ind len) << b << "prev")
           <|> restp
      setSessionData $ NextReg r

      where
      getData i len=  wraw . fromStr $ if i >= len || i < 0 then "" else show $ allnames !! i

      next i len = case i > len  of  True -> 0 ; _ -> i + n
      prev i len = case i < 0 of True -> len; _ -> i - n
