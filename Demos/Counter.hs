
module Counter ( counter, counterWidget) where
import MFlow.Wai.Blaze.Html.All
import Data.Monoid
import Data.String

attr= fromString
text= fromString

counter= do
   let explain=
        p << "This example emulates the"
        <> a ! href (attr "http://www.seaside.st/about/examples/counter") << "seaside counter example"
        <> p << "This widget uses a callback to permit an independent"
        <> p << "execution flow for each widget." <> a ! href (attr "/noscript/multicounter") << "Multicounter" <> (text " instantiate various counter widgets")
        <> p << "But while the seaside case the callback update the widget object, in this case"
        <> p << "the callback call generates a new copy of the counter with the value modified."

   ask $   explain
       ++> pageFlow "c" (counterWidget 0) <++ br
       <|> wlink () << p << "exit"

counterWidget n= 
      (h2 << show n     
       ++> wlink "i" << b << " ++ "
       <|> wlink "d" << b << " -- ")
      `wcallback`
        \op -> case op  of
          "i" -> counterWidget (n + 1)                       
          "d" -> counterWidget (n - 1)


-- to run it alone:
--main= runNavigation "" $ transientNav counter
