
module TestREST where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid
import Data.String

-- 9 pagems , each pagem has a restful link (pagem = ask)

-- to run it alone, replace askm by ask and uncomment the following line
--main= runNavigation "" $ transientNav testREST


testREST= do
  setTimeouts 120 0

  option <- pagem $   wlink "a" << b << "letters " <++ i << " or"
                 <|> wlink "1" << b << " numbers"

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
        p << "page for"
        <> b << x
        <> p << "goto next page"

