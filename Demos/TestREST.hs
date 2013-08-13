
module TestREST where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid
import Data.String

-- 9 pagems , each pagem has a restful link (pagem = ask)

-- to run it alone:
--main= runNavigation "" $ transientNav testREST


testREST= do
  setTimeouts 120 0

  addHeader header1

  option <- pagem $   wlink "a" << p << "letters " <++ p << "or"
                 <|> wlink "1" << p << "numbers"

  case option of
    "1" -> do
          pagem $ wlink "2" << contentFor "1"
          pagem $ wlink "3" << contentFor "2"
          pagem $ wlink "4" << contentFor "3"
          pagem $ wlink ()  << "menu"

    "a" -> do
          pagem $ wlink "b" << contentFor "a"
          pagem $ wlink "c" << contentFor "b"
          pagem $ wlink "d" << contentFor "c"
          pagem $ wlink ()  << "menu"


contentFor x=
        p << "pagem for"
        <> b << x
        <> p << "goto next page"

header1 h= html << body (text h)
  where
  text h= a ! href (fromString "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html")
            << "see this" <> hr <> h <> hr
          <> a ! href (fromString "/") << "main menu"

