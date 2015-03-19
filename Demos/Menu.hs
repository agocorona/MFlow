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


--import Debug.Trace
--(!>) = flip trace

newtype Filename= Filename String deriving Typeable

adminname= getConfig "cadmin" "admin"
edadmin= getConfig "edadmin" "editor"

-- present the widget w decorated with the main menu on the left and the source code at the bottom
page w= MF.ask $ do
    us <- getCurrentUser
    if us == anonymous then public else private 
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


ask= page


data Options= Wiki | CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select 
            | CheckBoxes | PreventBack | Multicounter
            | Combination | ShopCart | MCounter   | InitialConfig  | SearchCart
            | FViewMonad | Counter | WDialog |Push |PushDec |Trace | RESTNav
            | Database |  MFlowPersist   | AcidState
            | DatabaseSamples |PushSamples | ErrorTraces | Flows
            | BasicWidgets | MonadicWidgets | DynamicWidgets | LoginLogout
            | Templates | RuntimeTemplates | LoginWidget | CacheDataset
            | ComplexThings | GenerateForm | GenerateFormUndo | GenerateFormUndoMsg
            | LazyLoad
            deriving (Bounded, Enum,Read, Show,Typeable)


auto w= autoRefresh $ public >> maxAge 300 >> w 

mainMenu :: View Html IO Options
mainMenu= pageFlow "menu" $      
  ul<<<(li << a ! href "/" << b "HOME"
   ++> tFieldEd "editor" "othermenu"  "Other menu options"
   **> (li <<<  (absLink Wiki << b "Wiki") )
   <|> li << (b "About this menu" <> article cascade <> article menuarticle)
   ++> hr
   ++> ((auto $ li <<< do
          absLink BasicWidgets << b "Basic Widgets"
          ul <<<        
           (hr
           ++>(li <<< (absLink CountI << b "Increase an Int") <! noAutoRefresh
                       <++ b " A loop that increases the Int value of a text box"
                                   
           <|> li <<< (absLink CountS << b "Increase a String") <! noAutoRefresh
                       <++ b " A loop that concatenate text in a text box"
                                   
           <|> li <<< (absLink Select << b "Select options") <! noAutoRefresh
                       <++ b " A combo box"
                                   
           <|> li <<< (absLink CheckBoxes << b "Checkboxes") <! noAutoRefresh
           
           <|> li <<< (absLink Radio << b "Radio buttons") <! noAutoRefresh
           <++ hr)))

   <|> (auto $ li <<<   do
          absLink DynamicWidgets << b "Dynamic Widgets"
             <++ " Widgets with Ajax and containers of other widgets"
          ul <<<
           (hr
           ++>(li <<< (absLink Ajax         << b  "AJAX example")  <! noAutoRefresh
                 <++  " A onclick event in a text box invokes a server procedure that \
                          \increment the integer value"
                 <> article ajaxl

           <|> li <<< (absLink Autocomp     << b  "autocomplete")  <! noAutoRefresh
                 <++  " Example of autocomplete, a widget which takes the suggested values from a \
                 \ server procedure"  

           <|> li <<< (absLink AutocompList << b  "autocomplete List")   <! noAutoRefresh
                 <++  " Example of a widget that generates a set of return values, suggested by a \
                 \ autocomplete input box"
                 <> article editList

           <|> li <<< (absLink ListEdit     << b  "list edition")    <! noAutoRefresh
                 <++  " Example of a widget that edit, update and delete a list of user-defined \
                 \ widgets"

           <|> li <<< (absLink Grid         << b  "grid")    <! noAutoRefresh
                 <++  " Example of the same widget In this case, containing a row of two fields,\
                 \ aranged in a table"
                 <> article gridl
           <> hr)))

   <|> (auto $ li <<<   do
          absLink MonadicWidgets << b  "Monadic widgets, actions and callbacks"
             <++ " autoRefresh, page flows, dialogs etc"
          ul <<<                   
            (hr
            ++>(li <<< (absLink Action      << b  "Example of action") <! noAutoRefresh
                <++ " executed when a widget is validated"

            <|> li <<< (absLink FViewMonad   << b  "in page flow: sum of three numbers") <! noAutoRefresh
                 <++ " Page flows are monadic widgets that modifies themselves in the page"
                 <> article pageflow

            <|> li <<< (absLink Counter      << b  "Counter")  <! noAutoRefresh
                 <++ " A page flow which increases a counter by using a callback"
                 <> article callbacks

            <|> li <<< (absLink Multicounter << b  "Multicounter")  <! noAutoRefresh
                 <++ " Page flow with many independent counters with autoRefresh, so they modify themselves in-place"
                 <> article  callbacks

            <|> li <<< (absLink Combination  << b  "Combination of three dynamic widgets") <! noAutoRefresh
                 <++ " Combination of autoRefreshe'd widgets in the same page, with\
                          \ different behaviours"
                 <> article combinationl

            <|> li <<< (absLink WDialog      << b  "Modal dialog")   <! noAutoRefresh
                 <++ " A modal Dialog box with a form within a page flow"
            <> hr)))

   <|> (auto $ li <<< do
          absLink DatabaseSamples << b  "Database examples"
             <++ " with different backends"
          ul <<<
           (hr
           ++>(li <<< (absLink SearchCart <<  b  "Shopping with data tier, queries and full text search") <! noAutoRefresh
                <++  " The shopping example completed with a dynamic catalog stored using TCache"
                <> article searchcart

           <|> li <<< (absLink MFlowPersist <<  b "Persistent")  <! noAutoRefresh
                     <++ do -- blaze-html monad
                        " illustrates the use of MFlow with "
                        a  "Persistent" ! href yesodweb
                        " (In this example sqlite backend is used) "
                        article persistentarticle

           <|> li <<< (absLink Database << b  "Database") <! noAutoRefresh
                     <++ " Create, Store and retrieve lines of text from Amazon SimpleDB \
                            \ storage "
                     <> article amazonarticle
           <|> li <<< (absLink AcidState << b  "Acid State") <! noAutoRefresh
                     <++ do  -- blaze-html monad
                        " Create, Store and retrieve lines of text from "
                        a ! href "http://hackage.haskell.org/package/acid-state" $ "Acid State"
                        hr)))

   <|> (auto $ li <<<   do
          absLink PushSamples << b  "Push Samples"
             <++ " using long polling"
          ul <<<
           (hr
           ++>(li <<< (absLink Push << b  "Push example") <! noAutoRefresh
                     <++ " A push widget in append mode receives input from \
                             \a text box with autorefresh"
                     <> article pushl
                     
           <|>   li <<< (absLink PushDec << b  "A push counter") <! noAutoRefresh
                     <++ " Show a countdown. Then goes to the main menu"
                     <> article pushdec
                     <> hr)))

   <|> (auto $ li <<<   do
          absLink ErrorTraces << b  "Error Traces"
          ul <<<
            (hr
            ++>(li <<< (absLink Trace << b  " Execution traces for errors") <! noAutoRefresh
                 <++ " produces an error and show the complete execution trace"
                 <> article errorTrace
                 <> hr)))
                 
   <|> (auto $ li <<<   do
          absLink Flows << b  "Different kinds of flows"
          ul <<< 
           (hr
           ++>(li <<< (absLink RESTNav  << b  " REST navigation") <! noAutoRefresh
                <++ " Navigates trough  menus and a sucession of GET pages"
                <> article navigation


           <|> li <<< (absLink ShopCart <<  b  "Stateful persistent flow: shopping") <! noAutoRefresh
                <++ " Add articles to a persistent shopping cart stored in the session log."
                <> i " getSessionData is read in the View monad to get the most recent shopping cart\
                            \even when the back button has been pressed"
                <> article stateful

           <|> li <<< (absLink SearchCart <<  b  "Shopping with data tier, queries and full text search") <! noAutoRefresh
                <++ " The shopping example completed with a dynamic catalog stored using TCache"
                <> article searchcart

           <|> li <<< (absLink MCounter << b  "Persistent stateful flow: Counter") <! noAutoRefresh
                <++ " a persistent counter. It uses the same mechanism than shopping, but it is\
                      \a more simple example"

           <|> li <<< (absLink PreventBack  << b "Prevent going back after a transaction") <! noAutoRefresh
                 <++ " Control backtracking to avoid navigating back to undo something that can not be undone\
                          \. For example, a payment"
                 <> article preventbackl

           <|> li <<< (absLink InitialConfig  $ b "Initial Configuration in session parameters") <! noAutoRefresh
                 <++ " the user is asked for some questions initially that never will be asked again \
                       \ unless he likes to change them (all in session parameters)"

                 <> hr)))

  
   <|> (auto $ li <<< do
          absLink Templates << b "Runtime templates"
             <++ " Templates and content management modifiable at runtime"
          ul <<<
           (hr
           ++>(li <<< (absLink RuntimeTemplates << b "Runtime templates") <! noAutoRefresh
                  <++ " Example of form templates and result templates modified at runtime"
           <|> li <<< (absLink TextEdit << b "Content Management") <! noAutoRefresh
                  <++ " Example of content management primitives defined in MFlow.Forms.Widgets"
                  <> hr)))

   <|> (auto $ li <<< do
          absLink LoginLogout << b "Login/logout"
          ul <<< (hr ++> (li <<< (absLink Login << b  "login/logout")  <! noAutoRefresh
                             <++ " Example of using the login and/or logout"
                             <>  hr)))

   <|> (li <<< (absLink CacheDataset << b "HTTP caching")
           <++ " Navigating an infinite dataset in the browser by caching javascript programs\
               \ using the new composable caching directives")

   <|> li <<< absLink LazyLoad << b "Lazy loading of widgets, html, images etc"

   <|> (auto $ li <<< do 
          absLink ComplexThings << b "Really complex things" <++ " Reference impementations for GUI-like apps"
          ul <<< (hr
                 ++> (li <<< (absLink GenerateForm << b  "A form generator and editor")   <! noAutoRefresh
                             <++ " Add widgets and edit the layout. Execute the generated form and see the results")
                 <|> (li <<< (absLink GenerateFormUndo << b "Form generator with undo") <! noAutoRefresh
                             <++ " The same above application with undo edits thanks to the backtracking mechanism of MFlow")

                 <|> (li <<< (absLink GenerateFormUndoMsg << b "Form generator with no page refreshes") <! noAutoRefresh
                             <++ " The same above application with no page refresh for menu options. The form page show\
                                   \ validation errors and results via Ajax using witerate/dField"

                             <>  hr) ))

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
                  hr
                  disquscript
                     

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
    public >> maxAge 400 >> sMaxAge 300
    pagname <- restp <|> return "index"
    h1 ! At.style "text-align:center" <<<  tFieldEd "editor" (wikip ++pagname ++ "title.html")
                      (fromString pagname)
        **> tFieldEd "editor" (wikip ++ pagname ++ "body.html") "Enter the body"
        <++ do
           hr
           disquscript

  

  
wikip="wiki/"

disquscript= [shamlet|
    <div id="disqus_thread">
    <script type="text/javascript">
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = 'mflow'; // required: replace example with your forum shortname

        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();

    <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.
    <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus

    |]



wikientries= return .
 filter (isPrefixOf "lmth.eltit"  . reverse . fst) .
 filter  (isPrefixOf wikip . fst)  =<< indexOf tfieldKey

