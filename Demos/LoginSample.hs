
module LoginSample ( loginSample) where

import MFlow.Wai.Blaze.Html.All
import Menu
import Data.Monoid

loginSample= do
    r <- askm  $   p <<  "Please login with admin/admin"
             ++> userWidget (Just "admin") userLogin
             <|> wlink "exit" << p << "or exit"
        
    if r == "exit" then return () else do
        user <- getCurrentUser
    
        r <- askm  $   b <<  ("user logged as " <>  user)
                ++> wlink True  << p <<  "logout"
                <|> wlink False << p <<  "or exit"

        if r
          then do
             logout
             askm  $ p << "logged out" ++> wlink () << "press here to exit"
          else return ()
  

-- to run it alone, change askm by ask and uncomment this:
-- main= runNavigation "" $ transientNav loginSample
