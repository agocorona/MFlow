{-# OPTIONS -XCPP #-} 
module Multicounter ( multicounter) where

import Data.Monoid 
import Data.String
import Counter(counterWidget)
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav multicounter
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

text= fromString
attr= fromString

multicounter=
 page  $ explain
     ++> add (counterWidget 0) [1..4]
     <|> wlink () << p << "exit"

 where
 explain= p << "This example emulates the"
          <> a ! href (attr "http://www.seaside.st/about/examples/multicounter")
                << " seaside example"
          <> p << "It uses various copies of the " <> a ! href (attr "/noscript/counter") << "counter widget "
          <> text "instantiated in the same page. This is an example of how it is possible to "
          <> text "compose widgets with independent behaviours"


 add widget list= firstOf [autoRefresh $ pageFlow (show i) widget <++ hr | i <- list]


-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav multicounter
