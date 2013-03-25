module Main where
import MFlow.Wai.Blaze.Html.All

main= do
   addMessageFlows  [("" , transient $ runFlow pages)]
   wait $ run 8081 waiMessageFlow

pages=  do
     r<- ask landingPage
     case r of
       "myPage" -> do
         user <- ask loginPage
         ask $ userPage user


landingPage= p << "hi, this is the landing page"
               ++> wlink "myPage" << p << "press here to go to your page"
               <|> wlink "otherPage" << p << "or press here if you like to go to other page"

loginPage= userWidget Nothing userLogin

userPage user = p << ("this is your user page for " ++ user)
                  ++> wlink () << p << "press to go to landingPage"


