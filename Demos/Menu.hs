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
{-# LANGUAGE DeriveDataTypeable #-}
module Menu where
import Data.Typeable
import MFlow.Wai.Blaze.Html.All hiding (article, source)
import Text.Blaze.Html5 as El hiding (article, source)
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.String

newtype Filename= Filename String deriving Typeable

askm w= ask $ do
   Filename filename <- getSessionData `onNothing` error "source filename not set"
   (divmenu <<< br ++> retry mainMenu) 
    **> (El.div ! At.style (attr "float:right;width:65%;overflow:auto;") <<< source filename w)

divmenu= El.div ! At.style (attr "background-color:#EEEEEE;float:left\
                 \;width:30%\
                 \;margin-left:10px;margin-right:10px;overflow:auto;")

divsample= El.div ! At.style (attr "background-color:#FFEEEE;")

pagem= askm

attr= fromString
text= fromString

data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes | PreventBack | Multicounter
            | Combination | ShopCart | MCounter
            | FViewMonad | Counter | WDialog |Push |PushDec |Trace | RESTNav
            | Database
            deriving (Bounded, Enum,Read, Show,Typeable)

mainMenu= wcached "menu" 0 $
   b << "About this menu" ++> article menuarticle
   ++> br ++> br
   ++>b <<  "DATABASE"
   ++> (li <<< wlink Database << b << "Database"
                     <++ b << " Store, retrieve and edit lines of text in persistent storage ")
   <|> br 
   ++> b <<  "PUSH"
   ++> (li <<< wlink Push << b << "Push example"
                     <++ b << " A push widget in append mode receives input from\
                             \a text box with autorefresh"
                     <>  article pushl)
   <|> (li <<< wlink PushDec << b << "A push counter"
                     <++ b << " Show a countdown. Then goes to the main menu"
                     <>  article pushdec)
   <|> br ++> br
   ++> b <<  "ERROR TRACES"
   ++> (li <<< wlink Trace << b << " Execution traces for errors"
                 <++ b << " produces an error and show the complete execution trace"
                 <>  article errorTrace)
   <|> br ++> br ++>
   b <<  "DIFFERENT KINDS OF FLOWS"
               
   ++> (li <<< wlink RESTNav  << b <<  " REST navigation"
                <++ b << " Navigates trough  menus and a sucession of GET pages"
                <>  article navigation)


   <|> (li <<< wlink ShopCart  << b <<   "Stateful flow: shopping"
                <++ b << " Add articles to a persistent shopping cart"
                <>  article stateful)

   <|> (li <<< wlink MCounter << b <<   "Stateful flow: Counter"
                <++ b << " a persistent counter")
                       
   <|>  br ++> br ++> b <<  "BASIC"
               
   ++>  (li <<< wlink CountI       << b <<  "Increase an Int"
                       <++ b << " A loop that increases the Int value of a text box")
                                   
   <|>  (li <<< wlink CountS       << b <<  "Increase a String"
                       <++ b << " A loop that concatenate text in a text box")
                                   
   <|>  (li <<< wlink Select       << b <<  "Select options"
                       <++ b << " A combo box")
                                   
   <|>  (li <<< wlink CheckBoxes   << b <<  "Checkboxes")
           
   <|>  (li <<< wlink Radio        << b <<  "Radio buttons")

   <++  br <>  br                 <> b <<  "PAGE FLOWS with MONADIC WIDGETS, ACTIONS & CALLBACKS"
               
   <|>  (li <<< wlink Action      << b <<  "Example of action, executed when a widget is validated")

   <|>  (li <<< wlink FViewMonad   << b <<  "in page flow: sum of three numbers"
                 <++ b << " Page flows are monadic widgets that modifies themselves in the page"
                 <>  article pageflow)

   <|>  (li <<< wlink Counter      << b <<  "Counter"
                 <++ b << " A page flow which increases a counter by using a callback"
                 <>  article callbacks)

   <|>  (li <<< wlink Multicounter << b <<  "Multicounter"
                 <++ b << " Page flow with many independent counters with autoRefresh, so they modify themselves in-place"
                 <>  article callbacks)

   <|>  (li <<< wlink Combination  << b <<  "Combination of three dynamic widgets"
                 <++ b << " Combination of autoRefreshe'd widgets in the same page, with\
                          \ different behaviours"
                 <>  article combinationl)

   <|>  (li <<< wlink WDialog      << b <<  "Modal dialog"
                 <++ b << " A modal Dialog box with a form within a page flow")          

   <|>  br ++>  br               ++> b <<  "DYNAMIC WIDGETS"

   ++>  (li <<< wlink Ajax         << b <<  "AJAX example"
                 <++ b << " A onclick event in a text box invokes a server procedure that \
                          \increment the integer value"
                 <>  article ajaxl)

   <|>  (li <<< wlink Autocomp     << b <<  "autocomplete"
                 <++ b << " Example of autocomplete, a widget which takes the suggested values from a server procedure")                 

   <|>  (li <<< wlink AutocompList << b <<  "autocomplete List"
                 <++ b << " Example of a widget that generates a set of return values, suggested by a autocomplete input box"
                 <>  article editList)

   <|>  (li <<< wlink ListEdit     << b <<  "list edition"
                 <++ b << " Example of a widget that edit, update and delete a list of user-defined widgets")

   <|>  (li <<< wlink Grid         << b <<  "grid"
                 <++ b << " Example of the same widget In this case, containing a row of two fields, aranged in a table"
                 <>  article gridl)

   <|>  (li <<< wlink TextEdit     << b <<  "Content Management"
                 <++ b << " Example of the (basic) content management primitives defined in MFlow.Forms.Widgets")

   <|>  br ++>  br ++> b <<  "OTHERS"

   ++>  (li <<< wlink Login        << b <<  "login/logout"
                 <++ b << " Example of using the login and/or logout")

   <|>  (li <<< wlink PreventBack  << b <<  "Prevent going back after a transaction"
                 <++ b << " Control backtracking to avoid navigating back to undo something that can not be undone\
                          \. For example, a payment"
                 <>  article preventbackl)

space= preEscapedToMarkup "&nbsp;"
article link=  space <> a ! href (attr link) <<  i << "(article)"

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

source filename w =
      El.div <<< (divsample <<< w)
             <++ (do   -- Blaze-html monad
                  br
                  hr
                  h3 $ text "SOURCE CODE:"
                  iframe
                    ! At.style sty
                    ! At.height (attr "400")
                    ! At.src path
                    $ b $ text "no iframes")
      where
      host = "mflowdemo.herokuapp.com/"
      path = attr $ "http://" <> host <> filename
      download path= p $ text "download " <> a ! href  path << filename

      sty= attr "float:bottom\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"

stdheader  c= docTypeHtml $ do
   El.head $ do
     El.title << "MFlow examples"
     link ! rel   ( attr "stylesheet")
          ! type_ ( attr "text/css")
          ! href (attr "http://jqueryui.com/resources/demos/style.css")
   body $ do
      a ! At.style (attr "align:center;")
        ! href ( attr  "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html")
        $ h1 $ text "MFlow examples"
      p $ text "NOTE: heroku from time to time delete the database and the persistent flow state of these examples"
      hr
      c

   where
   sty= attr "float:left\
                \;width:100%\
                \;margin-left:5px;margin-right:10px;overflow:auto;"

showSource w filename = do
      setSessionData $ Filename filename
      w
