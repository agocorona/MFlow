{-# OPTIONS -F -pgmF MonadLoc #-}
module TraceSample ( traceSample) where

import MFlow.Wai.Blaze.Html.All hiding (page)
import Control.Monad.Loc

import Menu
-- to run it alone, remove import menu and uncomment the following line
--main= runNavigation "" $ transientNav traceSample


traceSample= do
  page $  h2 << "Error trace example"
       ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism"
       ++> p << "It is more detailed than a call stack"
       ++> p << "this example has a deliberate error"
       ++> br
       ++> p << "You must be logged as admin to see the trace"
       ++> wlink () << p << "pres here"

  page $   p <<  "Please login with admin/admin"
        ++> userWidget (Just "admin") userLogin

  u <- getCurrentUser
  page $   p << "The trace will appear after you press the link. press one of the options available at the bottom of the pagem"
        ++> p << ("user="++ u) ++> br
        ++> wlink () << "press here"
  page $   error $ "this is the error"

