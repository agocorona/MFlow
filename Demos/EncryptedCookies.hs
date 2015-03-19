{-# OPTIONS -XCPP #-}
module EncryptedLoginSample ( loginSample) where

import Data.Monoid
-- #define ALONE to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ step encryptedLoginSample
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

-- Expects files name CookieKey1.key, CookieKey2.key, CookieKey3.key,
-- and CookieKey4.key in project directory.

paranoidLoginSample = loginSample paranoidUserWidget paranoidLogout

-- Expects only a single file named CookieKey.key.

encryptedLoginSample = loginSample encryptedUserWidget encryptedLogout

loginSample widgetFunc logoutFunc = do
    userRegister "user" "user"
    r <- page  $   p <<  "Please login with user/user"
               ++> widgetFunc Nothing userLogin
               <|> wlink "exit" << p << "or exit"

    if r == "exit" then return () else do
        user <- getCurrentUser

        r <- page  $   b <<  ("user logged as " <>  user)
                   ++> wlink True  << p <<  "logout"
                   <|> wlink False << p <<  "or exit"

        if r
          then do
             logoutFunc
             page  $ p << "logged out" ++> wlink () << "press here to exit"
          else return ()
