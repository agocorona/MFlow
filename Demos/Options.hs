{-# OPTIONS -XCPP #-} 
module Options (options) where

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav options
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

options= do
   r <- page  $ getSelect (setSelectedOption ""  (p <<  "select a option") <|>
                         setOption "red"  (b <<  "red")                  <|>
                         setOption "blue" (b <<  "blue")                 <|>
                         setOption "Green"  (b <<  "Green")  )
                         <! dosummit
   page  $ p << (r ++ " selected") ++> wlink () (p <<  " menu")


   where
   dosummit= [("onchange","this.form.submit()")]

-- to run it alone, change page by ask and uncomment this:
--main= runNavigation "" $ transientNav options
