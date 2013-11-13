
module TestREST where
import MFlow.Wai.Blaze.Html.All hiding(page)
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

  option <- page $  h2 << "Choose between:"
                 ++> wlink "a" << b << "letters " <++ i << " or "
                 <|> wlink "1" << b << "numbers"

  case option of
    "1" -> do
          page $ wlink "2" << contentFor "1" 
          page $ wlink "3" << contentFor "2"
          page $ wlink "4" << contentFor "3"
          page $ wlink ()  << "menu"

    "a" -> do
          page $ wlink "b" << contentFor "a"
          page $ wlink "c" << contentFor "b"
          page $ wlink "d" << contentFor "c"
          page $ wlink ()  << "menu"


contentFor x= do
        p << "page for"
        b << x
        p << "goto next page"
        p << "or press the back button for the previous page"

