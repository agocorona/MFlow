{-# LINE 1 "INPUT" #-}
{-# OPTIONS -F -pgmF MonadLoc  #-}
module Demos.TraceSample (traceSample) where
{-# LINE 4 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All
{-# LINE 5 "INPUT" #-}
import Control.Monad.Loc
{-# LINE 7 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (7, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (8, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << "pres here")
          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (16, 3)" (page $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (20, 3)" (page $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (23, 3)" (page $ error $ "this is the error"))
