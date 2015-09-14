{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Prelude hiding (div)
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5.Attributes as At hiding (step,name)
import Data.Typeable

main = do
    userRegister "admin" "admin"
    runNavigation "app" $ transientNav mainPage


mainPage= lpage  $  "this is the main page" ++> noWidget


data TopNav= TopNav1 | TopNav2 | Admin deriving (Typeable, Show)

lpage content=  do
       r <- page   $ ( divtop <<< (Left <$> topNavigation <++ br))
                 <|> ( divleft <<< leftWidget)
                 <|> ( divcenter <<< (Right <$> content))
                 <|> ( divright <<< (Left <$> adminWidget))
       case r of
          Left navOptions      -> navigate navOptions
          Right contentOptions -> navigateContent contentOptions

divtop  = div !  At.style  "align:top"
divright= div ! At.style "float:right"
divleft= div ! At.style "float:left"
divcenter= div ! At.style "margin-left:20%"


adminWidget=
   authenticateWidget
   **> maybeLinkAdmin

maybeLinkAdmin= do
  username <- getCurrentUser
  if (username /= anonymous)
     then absLink Admin  "adminPage"
     else noWidget

topNavigation= absLink TopNav1 "top Navigation one" <|> " " ++>
               absLink TopNav2 "top Navigation two"

navigate Admin = do
     liftIO $ print "ADMINPAGE"
     lpage $ b "admin page" ++> noWidget

navigate TopNav1 =  do
     liftIO $ print "TOPNAV1"
     lpage $  b "top navigation one" ++> noWidget

navigate TopNav2 =  do
     liftIO $ print "TOPNAV2"
     lpage $  b "top navigation two" ++> noWidget

navigateContent _ = lpage $ "navigate Content" ++> noWidget

leftWidget=  "I'm the left widget" ++> noWidget

-- normally to be used with autoRefresh and pageFlow when used with other widgets.
authenticateWidget ::  View Html IO ()
authenticateWidget=  wform $ do
   username <- getCurrentUser
   if username /= anonymous
         then do
           private; noCache;noStore
           return username   -- if it is logged alredy, return username to the first wcallback
         else do             -- if not it tries to get the user/pass from the paramenters and log in if the user sent login/passw
          (name, pass) <- (,) <$> (getString Nothing <! hint "login name"
                                    <! size' (9 :: Int)
                                    <++ br)
                              <*> (getPassword <! hint "password"
                                    <! size' 9)
                              <++ br
                              <** submitButton "login"
          val  <- userValidate (name,pass)
          case val of
            Just msg -> notValid msg
            Nothing  -> login name >> clearEnv' >>  (return name)
   -- if one ohttp://mflowdemo.herokuapp.com/noscript/wiki/expressioneverf the two return the user, then wcallback erase the login/password boxes and present  logged as .... logout
   `wcallback` (\name -> ftag "b" (fromStr $ "logged as " ++ name++ " ")
                     ++> pageFlow "logout" (submitButton "logout")) -- wlink ("logout" :: String) (ftag "b" $ fromStr " logout"))

   -- the second callback is activated when  logout is pressed, and call wlogin to present the login/pass boxes again
   `wcallback`  const (logout >> clearEnv' >> authenticateWidget)

focus = [("onload","this.focus()")]
hint s= [("placeholder",s)]
size' n= [("size",show n)]
