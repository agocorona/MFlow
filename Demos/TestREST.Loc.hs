{-# LINE 1 "INPUT" #-}
{-# OPTIONS -F -pgmF MonadLoc  #-}
module TestREST where
{-# LINE 3 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All
{-# LINE 4 "INPUT" #-}
import Data.Monoid
{-# LINE 5 "INPUT" #-}
import Data.String
{-# LINE 6 "INPUT" #-}
import Control.Monad.Loc
{-# LINE 14 "INPUT" #-}
testREST
  = Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (14, 11)"
      (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (15, 3)" (setTimeouts 120 0)
          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (16, 3)" (liftIO $ print "start/restart")
          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (18, 3)" (setHeader header1)
          option <- Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (20, 3)" (page $ wlink "a" << p << "letters " <++ p << "or" <|> wlink "1" << p << "numbers")
          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (23, 3)"
            (case option of
                 "1" -> Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (24, 12)"
                          (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (25, 11)" (page $ wlink "2" << cont "1")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (26, 11)" (page $ wlink "3" << cont "2")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (27, 11)" (page $ wlink "4" << cont "3")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (28, 11)" (page $ wlink () << "menu"))
                 "a" -> Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (30, 12)"
                          (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (31, 11)" (page $ wlink "b" << cont "a")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (32, 11)" (page $ wlink "c" << cont "b")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (33, 11)" (page $ wlink "d" << cont "c")
                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (34, 11)" (page $ wlink () << "menu"))))
{-# LINE 37 "INPUT" #-}
cont x = p << "page for" <> b << x <> p << "goto next page"
{-# LINE 41 "INPUT" #-}
header1 h = html << body (text h)
  where {-# LINE 43 "INPUT" #-}
        text h = a ! href (fromString "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html") << "see this" <> hr <> h <> hr <> a ! href (fromString "/") << "main menu"
