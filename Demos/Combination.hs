{-# OPTIONS -XCPP #-} 
module Combination ( combination, wlogin1) where
import Counter(counterWidget)
import Data.String
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav combination
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

text= fromString

combination =  page $ 
     p << "Three active widgets in the same page with autoRefresh. Each widget refresh itself"
     ++> p << "with Ajax. If Ajax is not active, they will refresh by sending a new page."
     ++> hr 
     ++> p << "Login widget (use admin/admin)" ++>  pageFlow  "r" (autoRefresh wlogin1)  <++ hr
     **> p << "Counter widget" ++> pageFlow "c" (autoRefresh $ counterWidget 0 )  <++ hr
     **> p << "Dynamic form widget" ++> pageFlow "f" (autoRefresh formWidget) <++ hr
     **> wlink () << b << "exit"

formWidget :: View Html IO ()
formWidget=   do
      (n,s) <- (,) <$> p << "Who are you?"
                   ++> getString Nothing <! hint "name"     <++ br
                   <*> getString Nothing <! hint "surname"  <++ br
                   <** submitButton "ok" <++ br

      flag <- b << "Do you " ++> getRadio[radiob "work?",radiob "study?"] <++ br

      r<- case flag of
         "work?" -> pageFlow "l"
                     $ Left  <$> b << "do you enjoy your work? "
                             ++> getBool True "yes" "no"
                             <** submitButton "ok" <++ br

         "study?"-> pageFlow "r"
                     $ Right <$> b << "do you study in "
                             ++> getRadio[radiob "University"
                                         ,radiob "High School"]
      u <-  getCurrentUser
      p << ("You are "++n++" "++s)
        ++> p << ("And your user is: "++ u)
        ++> case r of
             Left fl ->   p << ("You work and it is " ++ show fl ++ " that you enjoy your work")
                            ++> noWidget

             Right stu -> p << ("You study at the " ++ stu)
                            ++> noWidget



hint s= [("placeholder",s)]
onClickSubmit= [("onclick","if(window.jQuery){\n"++
                                  "$(this).parent().submit();}\n"++
                           "else {this.form.submit()}")]
radiob s n= wlabel (text s) $ setRadio s n <! onClickSubmit



-- | If not logged, it present a page flow which page  for the user name, then the password if not logged
--
-- If logged, it present the user name and a link to logout
--
-- normally to be used with autoRefresh and pageFlow when used with other widgets.
wlogin1 :: View Html IO ()
wlogin1 =  do
   username <- getCurrentUser
   if username /= anonymous
         then return username
         else do
          name <- getString Nothing <! hint "username" <++ br
          pass <- getPassword <! hint "password" <** submitButton "login" <++ br
          val  <- userValidate (name,pass)
          case val of
            Just msg -> notValid msg
            Nothing  -> login name >> return name
       
   `wcallback` (\name -> b << ("logged as " ++ name)
                     ++> p << "navigate away of this page before logging out"
                     ++>  wlink "logout"  << b << " logout")
   `wcallback`  const (logout >> wlogin1)

focus = [("onload","this.focus()")]


