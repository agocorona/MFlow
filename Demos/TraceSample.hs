{-# OPTIONS -F -pgmF MonadLoc #-}
module TraceSample ( traceSample) where

import MFlow.Wai.Blaze.Html.All
import Menu
import Control.Monad.Loc

traceSample= do
  pagem $   h2 << "Error trace example"
       ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism"
       ++> p << "It is more detailed than a call stack"
       ++> p << "this example has a deliberate error"
       ++> br
       ++> p << "You must be logged as admin to see the trace"
       ++> wlink () << p << "pres here"

  pagem $   p <<  "Please login with admin/admin"
        ++> userWidget (Just "admin") userLogin

  u <- getCurrentUser
  pagem $   p << "The trace will appear after you press the link. press one of the options available at the bottom of the pagem"
        ++> p << ("user="++ u) ++> br
        ++> wlink () << "press here"
  pagem $   error $ "this is the error"

-- to run it alone, replace askm by ask and uncomment the following line
--main= runNavigation "" $ transientNav traceSample
