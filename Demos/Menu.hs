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
import MFlow.Wai.Blaze.Html.All hiding (article, source,page,ask)
import qualified MFlow.Wai.Blaze.Html.All as MF(page,ask)
import Text.Blaze.Html5 as El hiding (article, source)
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.Monoid
import Data.String
import Data.TCache.Memoization
import Data.TCache.IndexQuery
import Data.List(isPrefixOf)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Text.Hamlet
import System.IO.Unsafe


import Debug.Trace

(!>) = flip trace

newtype Filename= Filename String deriving Typeable

adminname= "admin"
edadmin= "editor"

-- present the widget w decorated with the main menu on the left and the source code at the bottom
ask w= MF.ask $ do
    filename <- getSessionData
    tFieldEd edadmin "head" "set Header"
       **> (El.div ! At.style "float:right" <<<   wlogin )
       <++ hr
       **> (divmenu <<< br ++>  retry  mainMenu)
       **> (El.div ! At.style "float:right;width:65%;overflow:auto;"
            <<< (insertForm $ widgetAndSource filename w))
  
divmenu= El.div
     ! At.style ("background-color:#EEEEEE;float:left\
                 \;width:30%;margin-left:10px;margin-right:10px;overflow:auto;")

--topLogin= El.div ! At.style "float:right;top:5px;left:5px"
--              <<< autoRefresh (pageFlow "login"  wlogin)


page= ask


data Options= Wiki | CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select 
            | CheckBoxes | PreventBack | Multicounter
            | Combination | ShopCart | MCounter   | InitialConfig  | SearchCart
            | FViewMonad | Counter | WDialog |Push |PushDec |Trace | RESTNav
            | Database |  MFlowPersist   | AcidState
            | DatabaseSamples |PushSamples | ErrorTraces | Flows
            | BasicWidgets | MonadicWidgets | DynamicWidgets | LoginLogout
            | Templates | RuntimeTemplates | LoginWidget
            | ComplexThings | GenerateForm | GenerateFormUndo
            deriving (Bounded, Enum,Read, Show,Typeable)




mainMenu :: View Html IO Options
mainMenu= pageFlow "" $
  ul<<<(li << a ! href "/" << b "HOME"
   ++> tFieldEd "editor" "othermenu"  "Other menu options"
   **> (li <<<  (absLink Wiki << b "Wiki") )
   <|> li << (b "About this menu" <> article cascade <> article menuarticle)
   ++> hr
   ++> ((autoRefresh $ li <<< do
          absLink BasicWidgets << b  "Basic Widgets"
          ul <<<        
           (hr
           ++> (li <<< (absLink CountI << b  "Increase an Int") <! noAutoRefresh
                       <++ b " A loop that increases the Int value of a text box"
                                   
           <|> li <<< (absLink CountS  << b  "Increase a String") <! noAutoRefresh
                       <++ b " A loop that concatenate text in a text box"
                                   
           <|> li <<< (absLink Select  << b  "Select options") <! noAutoRefresh
                       <++ b " A combo box"
                                   
           <|> li <<< (absLink CheckBoxes   << b  "Checkboxes") <! noAutoRefresh
           
           <|> li <<< (absLink Radio        << b  "Radio buttons") <! noAutoRefresh
           <++ hr)))

    <|> (autoRefresh $ li <<<   do
          absLink DynamicWidgets << b "Dynamic Widgets"
             <++ " Widgets with Ajax and containers of other widgets"
          ul <<<
           (hr
           ++>(li <<< (absLink Ajax         << b  "AJAX example")  <! noAutoRefresh
                 <++ b " A onclick event in a text box invokes a server procedure that \
                          \increment the integer value"
                 <> article ajaxl

           <|> li <<< (absLink Autocomp     << b  "autocomplete")  <! noAutoRefresh
                 <++ b " Example of autocomplete, a widget which takes the suggested values from a \
                 \ server procedure"  

           <|> li <<< (absLink AutocompList << b  "autocomplete List")   <! noAutoRefresh
                 <++ b " Example of a widget that generates a set of return values, suggested by a \
                 \ autocomplete input box"
                 <> article editList

           <|> li <<< (absLink ListEdit     << b  "list edition")    <! noAutoRefresh
                 <++ b " Example of a widget that edit, update and delete a list of user-defined \
                 \ widgets"

           <|> li <<< (absLink Grid         << b  "grid")    <! noAutoRefresh
                 <++ b " Example of the same widget In this case, containing a row of two fields,\
                 \ aranged in a table"
                 <> article gridl
           <> hr)))

    <|> (autoRefresh $ li <<<   do
          absLink MonadicWidgets << b  "Monadic widgets, actions and callbacks"
             <++ " autoRefresh, page flows, dialogs etc"
          ul <<<                   
            (hr
            ++>(li <<< (absLink Action      << b  "Example of action") <! noAutoRefresh
                <++ " executed when a widget is validated"

            <|> li <<< (absLink FViewMonad   << b  "in page flow: sum of three numbers") <! noAutoRefresh
                 <++ b " Page flows are monadic widgets that modifies themselves in the page"
                 <> article pageflow

            <|> li <<< (absLink Counter      << b  "Counter")  <! noAutoRefresh
                 <++ b " A page flow which increases a counter by using a callback"
                 <> article callbacks

            <|> li <<< (absLink Multicounter << b  "Multicounter")  <! noAutoRefresh
                 <++ b " Page flow with many independent counters with autoRefresh, so they modify themselves in-place"
                 <> article  callbacks

            <|> li <<< (absLink Combination  << b  "Combination of three dynamic widgets") <! noAutoRefresh
                 <++ b " Combination of autoRefreshe'd widgets in the same page, with\
                          \ different behaviours"
                 <> article combinationl

            <|> li <<< (absLink WDialog      << b  "Modal dialog")   <! noAutoRefresh
                 <++ b " A modal Dialog box with a form within a page flow"
            <> hr)))

   <|> (autoRefresh $ li <<< do
          absLink DatabaseSamples << b  "Database examples"
             <++ " with different backends"
          ul <<<
           (hr
           ++>(li <<< (absLink SearchCart <<  b  "Shopping with data tier, queries and full text search") <! noAutoRefresh
                <++ b " The shopping example completed with a dynamic catalog stored using TCache"
                <> article searchcart

           <|> li <<< (absLink MFlowPersist <<  b "Persistent")  <! noAutoRefresh
                     <++ do -- blaze-html monad
                        b " illustrates the use of MFlow with "
                        a  "Persistent" ! href yesodweb
                        " (In this example sqlite backend is used) "
                        article persistentarticle

           <|> li <<< (absLink Database << b  "Database") <! noAutoRefresh
                     <++ b " Create, Store and retrieve lines of text from Amazon SimpleDB \
                            \ storage "
                     <> article amazonarticle
           <|> li <<< (absLink AcidState << b  "Acid State") <! noAutoRefresh
                     <++ do  -- blaze-html monad
                        b " Create, Store and retrieve lines of text from "
                        a ! href "http://hackage.haskell.org/package/acid-state" $ "Acid State"
                        hr)))

   <|> (autoRefresh $ li <<<   do
          absLink PushSamples << b  "Push Samples"
             <++ " using long polling"
          ul <<<
           (hr
           ++>(li <<< (absLink Push << b  "Push example") <! noAutoRefresh
                     <++ b " A push widget in append mode receives input from \
                             \a text box with autorefresh"
                     <> article pushl
                     
           <|>   li <<< (absLink PushDec << b  "A push counter") <! noAutoRefresh
                     <++ b " Show a countdown. Then goes to the main menu"
                     <> article pushdec
                     <> hr)))

   <|> (autoRefresh $ li <<<   do
          absLink ErrorTraces << b  "Error Traces"
          ul <<<
            (hr
            ++>(li <<< (absLink Trace << b  " Execution traces for errors") <! noAutoRefresh
                 <++ b " produces an error and show the complete execution trace"
                 <> article errorTrace
                 <> hr)))
                 
   <|> (autoRefresh $ li <<<   do
          absLink Flows << b  "Different kinds of flows"
          ul <<< 
           (hr
           ++>(li <<< (absLink RESTNav  << b  " REST navigation") <! noAutoRefresh
                <++ b " Navigates trough  menus and a sucession of GET pages"
                <> article navigation


           <|> li <<< (absLink ShopCart <<  b  "Stateful persistent flow: shopping") <! noAutoRefresh
                <++ b " Add articles to a persistent shopping cart stored in the session log."
                <> i " getSessionData is read in the View monad to get the most recent shopping cart\
                            \even when the back button has been pressed"
                <> article stateful

           <|> li <<< (absLink SearchCart <<  b  "Shopping with data tier, queries and full text search") <! noAutoRefresh
                <++ b " The shopping example completed with a dynamic catalog stored using TCache"
                <> article searchcart

           <|> li <<< (absLink MCounter << b  "Persistent stateful flow: Counter") <! noAutoRefresh
                <++ b " a persistent counter. It uses the same mechanism than shopping, but it is\
                      \a more simple example"

           <|> li <<< (absLink PreventBack  << b "Prevent going back after a transaction") <! noAutoRefresh
                 <++ b " Control backtracking to avoid navigating back to undo something that can not be undone\
                          \. For example, a payment"
                 <> article preventbackl

           <|> li <<< (absLink InitialConfig  $ b "Initial Configuration in session parameters") <! noAutoRefresh
                 <++ b " the user is asked for some questions initially that never will be asked again \
                       \ unless he likes to change them (all in session parameters)"

                 <> hr)))

  
   <|> (autoRefresh $ li <<< do
          absLink Templates << b "Runtime templates"
             <++ " Templates and content management modifiable at runtime"
          ul <<<
           (hr
           ++>(li <<< (absLink RuntimeTemplates << b "Runtime templates") <! noAutoRefresh
                  <++ b " Example of form templates and result templates modified at runtime"
           <|> li <<< (absLink TextEdit << b "Content Management") <! noAutoRefresh
                  <++ b " Example of content management primitives defined in MFlow.Forms.Widgets"
                  <> hr)))

   <|> (autoRefresh $ li <<< do
          absLink LoginLogout << b "Login/logout"
          ul <<< (hr ++> (li <<< (absLink Login << b  "login/logout")   <! noAutoRefresh
                             <++ b " Example of using the login and/or logout"
                             <>  hr)))

   <|> (autoRefresh $ li <<< do 
          absLink ComplexThings << b "Really complex things" <++ " Reference impementations for GUI-like apps"
          ul <<< (hr
                 ++> (li <<< (absLink GenerateForm << b  "A form generator and editor")   <! noAutoRefresh
                             <++ b " Add widgets and edit the layout. Execute the generated form and see the results")
                 <|> (li <<< (absLink GenerateFormUndo << b "form generator with undo") <! noAutoRefresh
                             <++ b " The same above application with undo edits thanks to the backtracking mechanism of MFlow"

                             <>  hr)))

   <++ li << (a ! href "/noscript/wiki/webservices" $ b "Web Services"))
                    
   <|> (El.div ! At.style "display:none" <<< mainMenu1))


 -- for compatibility with older paths published that did not have two-step cascaded menus
 -- so that /noscript/databasesampes/mflowpersist and /noscript/mflowpersist produce the same page

mainMenu1 :: View Html IO Options
mainMenu1= wcached "menu" 0 $
 wlink MFlowPersist     mempty
   <|> wlink Database   mempty
   <|> wlink Push       mempty
   <|> wlink PushDec    mempty
   <|> wlink Trace      mempty
   <|> wlink RESTNav    mempty
   <|> wlink ShopCart   mempty
   <|> wlink MCounter   mempty
   <|> wlink CountI       mempty                           
   <|> wlink CountS       mempty                     
   <|> wlink Select       mempty                      
   <|> wlink CheckBoxes   mempty
   <|> wlink Radio        mempty      
   <|> wlink Action       mempty
   <|> wlink FViewMonad   mempty
   <|> wlink Counter      mempty
   <|> wlink Multicounter mempty
   <|> wlink Combination  mempty
   <|> wlink WDialog      mempty
   <|> wlink Ajax         mempty
   <|> wlink Autocomp     mempty
   <|> wlink AutocompList mempty
   <|> wlink ListEdit     mempty
   <|>  wlink Grid         << b "grid"
   <|>  wlink TextEdit     << b "Content Management"
   <|>  wlink Login        << b "login/logout"
   <|>  wlink PreventBack  << b "Prevent going back after a transaction"


article link=  " " <> a ! At.class_ "_noAutoRefresh"  ! href link <<  i "(article)"

searchcart= "http://haskell-web.blogspot.com.es/2013/04/mflow-what-about-data-tier-adding-it-to.html"
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
cascade="http://haskell-web.blogspot.com.es/2013/10/a-cascade-menu-coded-in-pure.html"

widgetAndSource Nothing w = w
widgetAndSource (Just(Filename filename)) w = do
      source <- getSource filename
      El.div <<<  tFieldEd edadmin (filename ++ "top") "top text"
             <++ hr
             **> h1 "Running example"
             ++> "(in the light red box):"
             ++> (divsample <<< w)
--             <** tFieldEd edadmin (filename ++ "bottom") "botom text"
             <++ do -- Blaze-html monad
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
     El.meta ! content "text/html; charset=UTF-8" ! httpEquiv "Content-Type"

 
     El.style $ "body {\n\
            \font-family: \"rebuchet MS\", \"Helvetica\", \"Arial\",  \"Verdana\", \"sans-serif\";\n\
            \font-size: 90.5%;}\n\
            \a:link {text-decoration:none;}\
            \a:visited {text-decoration:none;}\
            \a:hover {text-decoration:underline;}\
            \a:active {text-decoration:underline;}"
            
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
                ++ hscolour HTML colourPrefs False True file False source
                ++ "</font>"
                
colourPrefs= unsafePerformIO readColourPrefs

wiki =  page $ do
    pagname <- getRestParam `onNothing` return "index"
    (docTypeHtml $ El.head $ El.title << pagname)
        ++> (El.body
        <<<   ( h1 ! At.style "text-align:center" <<<  tFieldEd "editor" (wikip ++pagname ++ "title.html") (fromString pagname))
        **>   tFieldEd "editor" (wikip ++ pagname ++ "body.html") "Enter the body"
--        <++ shareHtml
--        <>  shareScript
        )

wikip="wiki/"

wikientries= return . filter  (isPrefixOf wikip . fst)  =<< indexOf tfieldKey

shareScript=  [shamlet|<script>
(function(doc, script) {
  var js,
      fjs = doc.getElementsByTagName(script)[0],
      frag = doc.createDocumentFragment(),
      add = function(url, id) {
          if (doc.getElementById(id)) {return;}
          js = doc.createElement(script);
          js.src = url;
          id && (js.id = id);
          frag.appendChild( js );
      };

    // Google+ button
    add('http://apis.google.com/js/plusone.js');
    // Facebook SDK
    add('//connect.facebook.net/en_US/all.js#xfbml=1&appId=200103733347528', 'facebook-jssdk');
    // Twitter SDK
    add('//platform.twitter.com/widgets.js');

    fjs.parentNode.insertBefore(frag, fjs);
}(document, 'script'));

|]

shareHtml= [shamlet|<a href="https://twitter.com/share" class="twitter-share-button" data-count="horizontal">Tweet</a>

<div class="g-plusone" data-size="medium" data-count="true"></div>

<div id="fb-root"></div>
<div class="fb-like" data-send="false" data-layout="button_count" data-width="1" data-show-faces="false" data-action="recommend"></div>
|]
