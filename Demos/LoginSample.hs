
module LoginSample ( loginSample) where

import MFlow.Wai.Blaze.Html.All
import Data.Monoid

loginSample= do
    r <- ask $   p <<  "Please login with admin/admin"
             ++> userWidget (Just "admin") userLogin
             <|> wlink "exit" << p << "or exit"
        
    if r == "exit" then return () else do
        user <- getCurrentUser
    
        r <- ask $   b <<  ("user logged as " <>  user)
                ++> wlink True  << p <<  "logout"
                <|> wlink False << p <<  "or exit"

        if r
          then do
             logout
             ask $ p << "logged out" ++> wlink () << "press here to exit"
          else return ()
  

-- to run it alone:
-- main= runNavigation "" $ transientNav loginSample
