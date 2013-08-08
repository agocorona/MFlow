{-# OPTIONS -F -pgmF MonadLoc #-}
module TraceSample ( traceSample) where

import MFlow.Wai.Blaze.Html.All
import Control.Monad.Loc

traceSample= do
  page $   h2 << "Error trace example"
       ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism"
       ++> p << "It is more detailed than a call stack"
       ++> p << "this example has a deliberate error"
       ++> br
       ++> p << "You must be logged as admin to see the trace"
       ++> wlink () << p << "pres here"

  page $   p <<  "Please login with admin/admin"
       ++> userWidget (Just "admin") userLogin

  u <- getCurrentUser
  page $   p << "The trace will appear after you press the link. press one of the options available at the bottom of the page"
       ++> p << ("user="++ u) ++> br
       ++> wlink () << "press here"
  page $   error $ "this is the error"

-- to run it alone:
--main= runNavigation "" $ transientNav traceSample
