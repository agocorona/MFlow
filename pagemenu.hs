{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Prelude hiding (div)
import MFlow.Wai.Blaze.Html.All hiding(content)
import Text.Blaze.Html5.Attributes as At hiding (step,name,content)
import Data.Typeable

main = do
    userRegister "admin" "admin"
    runNavigation "app" $ transientNav  $ lpage topMessage


topMessage=   "this is the top message" ++> noWidget


data TopNav= TopNav1 | TopNav2 | Admin deriving (Typeable, Show)

lpage cont= page $ do
       r <-     ( divtop <<< (Left <$> topNavigation <++ br))
                 <|> ( divleft <<< leftWidget)
                 <|> ( divcenter <<< (Right <$> cont))
                 <|> ( divright <<< (Left <$> adminWidget))
       case r of
          Left navOptions      -> content navOptions
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

topNavigation= absLink TopNav1 "top link one" <|> " " ++>
               absLink TopNav2 "top link two"

content Admin = do
     liftIO $ print "ADMINPAGE"
     b "admin content" ++> noWidget

content TopNav1 =  do
     liftIO $ print "TOPNAV1"
     b "top link one content" ++> noWidget

content TopNav2 =  do
     liftIO $ print "TOPNAV2"
     b "top link two content" ++> noWidget

navigateContent _ = "navigate Content" ++> noWidget

leftWidget=  "I'm the left widget" ++> noWidget

-- normally to be used with autoRefresh and pageFlow when used with other widgets.
authenticateWidget ::  View Html IO ()
authenticateWidget=  wform $ do
   liftIO $ print "AUTH"
   username <- getCurrentUser
   if username /= anonymous
         then do
              liftIO $ print "was logged"
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
            Nothing  -> login name >> clearEnv' >> (return name)
   -- if one ohttp://mflowdemo.herokuapp.com/noscript/wiki/expressioneverf the two return the user, then wcallback erase the login/password boxes and present  logged as .... logout
   `wcallback` (\name -> do
           private; noCache;noStore
           liftIO $ print "first callback"
           ftag "b" (fromStr $ "logged as " ++ name++ " ")
                     ++> pageFlow "logout" (submitButton "logout")) -- wlink ("logout" :: String) (ftag "b" $ fromStr " logout"))

   -- the second callback is activated when  logout is pressed, and call wlogin to present the login/pass boxes again
   `wcallback`  const (liftIO (print "second callback") >> logout >> clearEnv' >> authenticateWidget)

focus = [("onload","this.focus()")]
hint s= [("placeholder",s)]
size' n= [("size",show n)]
