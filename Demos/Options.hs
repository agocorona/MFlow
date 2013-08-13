
module Options (options) where

import MFlow.Wai.Blaze.Html.All
import Menu

options= do
   r <- askm  $ getSelect (setSelectedOption ""  (p <<  "select a option") <|>
                         setOption "red"  (b <<  "red")                  <|>
                         setOption "blue" (b <<  "blue")                 <|>
                         setOption "Green"  (b <<  "Green")  )
                         <! dosummit
   askm  $ p << (r ++ " selected") ++> wlink () (p <<  " menu")


   where
   dosummit= [("onchange","this.form.submit()")]

-- to run it alone, change askm by ask and uncomment this:
--main= runNavigation "" $ transientNav options
