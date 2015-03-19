{-# OPTIONS  -XCPP #-}
module TestREST(testREST) where
import Data.Monoid
import Data.String



-- to execute it alone, uncomment this
-- #define ALONE
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $  transientNav testREST
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

-- A menu with two branches, each one is a sucession of pages

-- 9 pages , each page has a restful link (pagem = ask)

-- to run it alone,  delete "import Menu" and uncomment the next lines

-- main= runNavigation "" $ transientNav testREST

-- pagem= page


testREST= do
  option <- page options

  case option  of
    "1" -> do
          page $ wlink "2" << contentFor "1" 
          page $ wlink "3" << contentFor "2"
          page $ wlink "4" << contentFor "3"
          page $ optionsorexit
          return () 
     
    "a" -> do
          page $ wlink "b" << contentFor "a"
          page $ wlink "c" << contentFor "b"
          page $ wlink "d" << contentFor "c"
          page $ optionsorexit
          return ()

options= h2 << "Choose between:"
         ++> absLink "a" << b << "letters " <++ i << " or "
         <|> absLink "1" << b << "numbers"

optionsorexit= options <|> wlink "home" << p << " or go to home"
          -- if home is pressed, will return(). Otherwise, it will backtrack to one of the
          -- two options since they are abslinks.

contentFor x= do
        p << "page for"
        b << x
        p << "goto next page"
        p << "or press the back button for the previous page"

