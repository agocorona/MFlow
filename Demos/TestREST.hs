
module TestREST where
import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid
import Data.String

-- A menu with two branches, each one is a sucession of pages

-- 9 pages , each page has a restful link (pagem = ask)

-- to run it alone,  delete "import Menu" and uncomment the next lines

-- main= runNavigation "" $ transientNav testREST

-- pagem= page


testREST= do
--  setTimeouts 120 0

  option <- pagem $  h2 << "Choose between:"
                 ++> wlink "a" << b << "letters " <++ i << " or "
                 <|> wlink "1" << b << "numbers"

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


contentFor x= do
        p << "page for"
        b << x
        p << "goto next page"
        p << "or press the back button for the previous page"

