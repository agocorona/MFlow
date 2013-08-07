
module PreventGoingBack ( preventBack) where
import MFlow.Wai.Blaze.Html.All
import System.IO.Unsafe
import Control.Concurrent.MVar

rpaid= unsafePerformIO $ newMVar (0 :: Int)


preventBack= do
    ask $ wlink () << b << "press here to pay 100000 $ "
    payIt
    paid  <- liftIO $ readMVar rpaid
    preventGoingBack . ask $   p << "You already paid 100000 before"
                           ++> p << "you can no go back until the end of the buy process"
                           ++> wlink () << p << "Please press here to continue"
                           
    ask $   p << ("you paid "++ show paid)
        ++> wlink () << p << "Press here to go to the menu or press the back button to verify that you can not pay again"
    where
    payIt= liftIO $ do
      print "paying"
      paid <- takeMVar  rpaid
      putMVar rpaid $ paid + 100000

-- to run it alone:
--main= runNavigation "" $ transientNav preventBack
