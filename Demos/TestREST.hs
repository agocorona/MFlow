module TestREST where
import MFlow.Wai.Blaze.Html.All
import Data.Monoid

-- 9 pages , each page has a restful link (page = ask)

-- to run it alone:
--main= runNavigation "" $ transientNav testREST


testREST= do
  liftIO $ print "start/restart"

  setHeader $ html . body

  option <- page $   wlink "a" << p << "letters " <++ p << "or"
                 <|> wlink "1" << p << "numbers"

  case option of
    "1" -> do
          page $ wlink "2" << cont "1"
          page $ wlink "3" << cont "2"
          page $ wlink "4" << cont "3"
          page $ wlink ()  <<  "menu"

    "a" -> do
          page $ wlink "b" << cont "a"
          page $ wlink "c" << cont "b"
          page $ wlink "d" << cont "c"
          page $ wlink ()  <<  "menu"


cont x= p << "page for"
        <> b << x
        <> p << "goto next page"
