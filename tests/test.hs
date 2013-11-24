{-# LANGUAGE   OverloadedStrings #-}
import MFlow.Wai.Blaze.Html.All hiding (footer)

import Data.Monoid
import Control.Applicative
import Control.Concurrent
import Control.Workflow(Workflow)

import Debug.Trace

(!>) = flip trace

footer= a ! href "http://haskell.org"  $ img ! src "haskell-logo-revolution.png"
main2= runNavigation "" $ step $ do
  setFilesPath "tests/"
  ask $ wlink () "press here" <++ footer
  r <- ask $ getString Nothing
  ask $ wlink () (b << r)  <++ footer

main= runNavigation "" . step $ do
  r <- page $ wlink True "reserve or purchase a flight" <|> wlink False "other options"
  case r of
    True -> runFlowIn "reserve" reserve
    False -> other


other= page $ "doing other things" ++> wlink() "end"

reserve :: FlowM Html (Workflow IO) ()
reserve = do
     step $ reserveFlight `onCancel` cancelReservation
     step $ manageAprobal
     step $ purchaseFlight

onCancel x y = do
  back <- goingBack
  case back of
     True -> y
     _    -> x

reserveFlight = do
   liftIO $ print  "flight reserved "
   breturn()

cancelReservation= do
   liftIO $ print  "reservation cancelled"
   breturn()

manageAprobal :: FlowM Html IO ()
manageAprobal= do
   liftIO $ threadDelay 10000000
   fail ""

purchaseFlight= do
   page $ wlink () "do you want to purchaase?"
   page $ b  "fligh purchased" ++> wlink () "finish"
