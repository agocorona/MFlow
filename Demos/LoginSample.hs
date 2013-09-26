
module LoginSample ( loginSample) where

import MFlow.Wai.Blaze.Html.All
import Data.Monoid
import Menu
-- to run it alone, comment import Menu and uncomment this:
--main= runNavigation "" $ transientNav loginSample
--askm= ask

loginSample= do
    userRegister "user" "user"
    r <- askm  $   p <<  "Please login with user/user"
               ++> userWidget Nothing userLogin
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
  


