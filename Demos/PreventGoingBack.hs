{-# OPTIONS -XCPP #-} 
module PreventGoingBack ( preventBack) where
import System.IO.Unsafe
import Control.Concurrent.MVar

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav preventBack
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

rpaid= unsafePerformIO $ newMVar (0 :: Int)


preventBack= do
    page  $ wlink "don't care" << b << "press here to pay 100000 $ "
    payIt
    paid  <- liftIO $ readMVar rpaid
    preventGoingBack . page  $ p << "You already paid 100000 before"
                           ++> p << "you can not go back until the end of the buy process"
                           ++> wlink () << p << "Please press here to continue"
                           
    page  $   p << ("you paid "++ show paid)
        ++> wlink () << p << "Press here to go to the menu or press the back button to verify that you can not pay again"
    where
    payIt= liftIO $ do
      print "paying"
      paid <- takeMVar  rpaid
      putMVar rpaid $ paid + 100000

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav preventBack
