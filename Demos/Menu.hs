-----------------------------------------------------------------------------
--
-- Module      :  Menu
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | This is the menu shared by all the demo modules of demos-blaze.hs
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings , QuasiQuotes #-}
module Menu where
import Data.Typeable
import MFlow.Wai.Blaze.Html.All hiding (article, source)
import Text.Blaze.Html5 as El hiding (article, source)
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.String
import Data.TCache.Memoization
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Hamlet


newtype Filename= Filename String deriving Typeable

adminname= "admin"
edadmin= "editor"

-- present the widget w decorated with the main menu on the left and the source code at the bottom
askm w= ask $ do
   Filename filename <- getSessionData `onNothing` error "source filename not set"
   tFieldEd edadmin "head" "set Header"
       <++ hr
       **> (divmenu <<< br ++> retry mainMenu)
       **> (El.div ! At.style "float:right;width:65%;overflow:auto;"
       <<< widgetAndSource filename w)
  
divmenu= El.div
     ! At.style ( "background-color:#EEEEEE;float:left\
                 \;width:30%\
                 \;margin-left:10px;margin-right:10px;overflow:auto;")



pagem= askm


data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes | PreventBack | Multicounter
            | Combination | ShopCart | MCounter
            | FViewMonad | Counter | WDialog |Push |PushDec |Trace | RESTNav
            | Database | MFlowPersist
            deriving (Bounded, Enum,Read, Show,Typeable)

mainMenu :: View Html IO Options
mainMenu= wcached "menu" 0 $
  ul <<<(
   li << b "About this menu" ++> article menuarticle
   ++> br
   ++> br
   ++> b "DATABASE"
   ++> (li <<< wlink MFlowPersist << b "Persistent"
                     <++ b " illustrates the use of MFlow with "
                     <> a  "Persistent" ! href yesodweb
                     <> " (In this example sqlite backend is used) "
                     <> article persistentarticle)
   <|> (li <<< wlink Database << b "Database"
                     <++ b " Create, Store and retrieve lines of text from Amazon SimpleDB storage "
                     <> article amazonarticle)
   <|> br 
   ++> b "PUSH"
   ++> (li <<< wlink Push << b "Push example"
                     <++ b " A push widget in append mode receives input from\
                             \a text box with autorefresh"
                     <> article pushl)
                     
   <|> (li <<< wlink PushDec << b "A push counter"
                     <++ b " Show a countdown. Then goes to the main menu"
                     <> article pushdec)
   <|> br ++> br
   ++> b "ERROR TRACES"
   ++> (li <<< wlink Trace << b " Execution traces for errors"
                 <++ b " produces an error and show the complete execution trace"
                 <> article errorTrace)
   <|> br ++> br ++>
   b "DIFFERENT KINDS OF FLOWS"
               
   ++> (li <<< wlink RESTNav  << b " REST navigation"
                <++ b " Navigates trough  menus and a sucession of GET pages"
                <> article navigation)


   <|> (li <<< wlink ShopCart  << b "Stateful persistent flow: shopping"
                <++ b " Add articles to a persistent shopping cart stored in the session log."
                <> i " getSessionData is read in the View monad to get the most recent shopping cart\
                            \even when the back button has been pressed"
                <> article stateful)

   <|> (li <<< wlink MCounter << b "Persistent stateful flow: Counter"
                <++ b " a persistent counter. It uses the same mechanism than shopping, but it is a more simple example")
                       
   <|>  br ++> br ++> b "BASIC"
               
   ++>  (li <<< wlink CountI       << b "Increase an Int"
                       <++ b " A loop that increases the Int value of a text box")
                                   
   <|>  (li <<< wlink CountS       << b "Increase a String"
                       <++ b " A loop that concatenate text in a text box")
                                   
   <|>  (li <<< wlink Select       << b "Select options"
                       <++ b " A combo box")
                                   
   <|>  (li <<< wlink CheckBoxes   << b "Checkboxes")
           
   <|>  (li <<< wlink Radio        << b "Radio buttons")

   <++  br <> br                  <> b "PAGE FLOWS with MONADIC WIDGETS, ACTIONS & CALLBACKS"
               
   <|>  (li <<< wlink Action      << b "Example of action, executed when a widget is validated")

   <|>  (li <<< wlink FViewMonad   << b "in page flow: sum of three numbers"
                 <++ b " Page flows are monadic widgets that modifies themselves in the page"
                 <> article pageflow)

   <|>  (li <<< wlink Counter      << b "Counter"
                 <++ b " A page flow which increases a counter by using a callback"
                 <> article callbacks)

   <|>  (li <<< wlink Multicounter << b "Multicounter"
                 <++ b " Page flow with many independent counters with autoRefresh, so they modify themselves in-place"
                 <> article callbacks)

   <|>  (li <<< wlink Combination  << b "Combination of three dynamic widgets"
                 <++ b " Combination of autoRefreshe'd widgets in the same page, with\
                          \ different behaviours"
                 <> article combinationl)

   <|>  (li <<< wlink WDialog      << b "Modal dialog"
                 <++ b " A modal Dialog box with a form within a page flow")          

   <|>  br ++>  br               ++> b "DYNAMIC WIDGETS"

   ++>  (li <<< wlink Ajax         << b "AJAX example"
                 <++ b " A onclick event in a text box invokes a server procedure that \
                          \increment the integer value"
                 <> article ajaxl)

   <|>  (li <<< wlink Autocomp     << b "autocomplete"
                 <++ b " Example of autocomplete, a widget which takes the suggested values from a server procedure")                 

   <|>  (li <<< wlink AutocompList << b "autocomplete List"
                 <++ b " Example of a widget that generates a set of return values, suggested by a autocomplete input box"
                 <> article editList)

   <|>  (li <<< wlink ListEdit     << b "list edition"
                 <++ b " Example of a widget that edit, update and delete a list of user-defined widgets")

   <|>  (li <<< wlink Grid         << b "grid"
                 <++ b " Example of the same widget In this case, containing a row of two fields, aranged in a table"
                 <> article gridl)

   <|>  (li <<< wlink TextEdit     << b "Content Management"
                 <++ b " Example of the (basic) content management primitives defined in MFlow.Forms.Widgets")

   <|>  br ++>  br ++> b "OTHERS"

   ++>  (li <<< wlink Login        << b "login/logout"
                 <++ b " Example of using the login and/or logout")

   <|>  (li <<< wlink PreventBack  << b "Prevent going back after a transaction"
                 <++ b " Control backtracking to avoid navigating back to undo something that can not be undone\
                          \. For example, a payment"
                 <> article preventbackl)

   )

article link=  " " <> a ! href ( link) <<  i "(article)"

persistentarticle= "http://haskell-web.blogspot.com.es/2013/08/mflow-using-persistent-with-sqlite.html"
yesodweb= "http://www.yesodweb.com/book/persistent"
amazonarticle= "http://haskell-web.blogspot.com.es/2013/08/using-amazon-web-services-with-tcache.html"
pushdec= "http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html"
pushl= "http://haskell-web.blogspot.com.es/2013/07/new-push-widgets-for-presentation-of.html"
errorTrace= "http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html"
navigation= "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html"
combinationl= "http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html"
pageflow= "http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html"
callbacks= "http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html"
gridl= "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html"
editList= "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html"
stateful= "http://haskell-web.blogspot.com.es/2013/04/more-on-session-management-in-mflow.html"
preventbackl= "http://haskell-web.blogspot.com.es/2013/04/controlling-backtracking-in-mflow.html"
ajaxl= "http://hackage.haskell.org/packages/archive/MFlow/0.3.1.0/doc/html/MFlow-Forms.html#g:17"
menuarticle= "http://haskell-web.blogspot.com.es/2013/08/how-to-handle-menus-and-other.html"

widgetAndSource filename w = do
      source <- getSource filename
      El.div <<< h1 "Running example"
             ++> "(in the ligth red box):"
             ++> (divsample <<< w)
             <** tFieldEd edadmin (filename ++ "top") "top text"
             <** tFieldEd edadmin (filename ++ "bottom") "botom text"


             <++  do -- Blaze-html monad
                  br
                  hr
                  h1 $ "Source code:"
                  source

      where
      host = "mflowdemo.herokuapp.com/"
      path = "http://" <> host <> "source/" <> filename
      download path= p $  "download " <> a ! href  path << filename

      sty=  "float:bottom\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"

divsample= El.div ! At.style ( "background-color:#FFEEEE;")

stdheader  c= docTypeHtml $ do
   El.head $ do
     El.title "MFlow examples"
     link ! rel   "stylesheet"
          ! type_ "text/css"
          ! href ( "http://jqueryui.com/resources/demos/style.css")
   body  $ do
      [shamlet|
         <script>
          (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
          m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
          })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

          ga('create', 'UA-95693-6', 'mflowdemo.herokuapp.com');
          ga('send', 'pageview');

      |]

      c

   where
   sty=  "float:left\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"

showSource w filename = do
   setSessionData $ Filename filename
   w


getSource file = liftIO $ cachedByKey file 0 $ do
   source <- readFile $ "Demos/" ++ file
   return . preEscapedToHtml
                $  "<font size=2>"
                ++ hscolour HTML defaultColourPrefs False True file False source
                ++ "</font>"

