{-# OPTIONS -XCPP -XDeriveDataTypeable #-} 
module Counter ( counter, counterWidget) where
import Data.Monoid
import Data.String
import Data.Typeable

-- #define ALONE -- to execute it alone, uncomment this

#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ step counter
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

attr= fromString
text= fromString

counter= do
   
   page  $ explain
       ++>  pageFlow "c" (counterWidget 0) <++ br
       <|> wlink () << p << "exit"
   where
   explain= do   -- using the blaze-html monad
        p << "This example emulates the"
        a ! href (attr "http://www.seaside.st/about/examples/counter") << "seaside counter example"
        p << "This widget uses a \"callback\" that permit an independent"
        text " execution flow for each widget."
        a ! href (attr "/noscript/multicounter") << "Multicounter"
        text " instantiate various counter widgets"
        p << "But while the seaside case the callback update the widget object, in this case"
        p << "the callback recursvely generates a new copy of the counter with the value modified."



counterWidget n=
      (h2 << show n
       ++> wlink "i" << b << " ++ "
       <|> wlink "d" << b << " -- ")
      `wcallback`
        \op -> case op  of
          "i" -> counterWidget (n + 1)
          "d" -> counterWidget (n - 1)

