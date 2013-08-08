
module Options (options) where

import MFlow.Wai.Blaze.Html.All

options= do
   r <- ask $ getSelect (setSelectedOption ""  (p <<  "select a option") <|>
                         setOption "red"  (b <<  "red")                  <|>
                         setOption "blue" (b <<  "blue")                 <|>
                         setOption "Green"  (b <<  "Green")  )
                         <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p <<  " menu")


   where
   dosummit= [("onchange","this.form.submit()")]

-- to run it alone:
--main= runNavigation "" $ transientNav options
