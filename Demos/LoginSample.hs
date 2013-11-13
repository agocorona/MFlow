{-# OPTIONS -XCPP #-} 
module LoginSample ( loginSample) where

import Data.Monoid
-- #define ALONE to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav autocomplete1
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

loginSample= do
    userRegister "user" "user"
    r <- page  $   p <<  "Please login with user/user"
               ++> userWidget Nothing userLogin
               <|> wlink "exit" << p << "or exit"
        
    if r == "exit" then return () else do
        user <- getCurrentUser
    
        r <- page  $   b <<  ("user logged as " <>  user)
                   ++> wlink True  << p <<  "logout"
                   <|> wlink False << p <<  "or exit"

        if r
          then do
             logout
             page  $ p << "logged out" ++> wlink () << "press here to exit"
          else return ()
  


