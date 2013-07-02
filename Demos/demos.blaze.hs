{-# LANGUAGE  DeriveDataTypeable #-}
module Main where
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String
import Data.List
import Data.TCache
import Data.Typeable
import Control.Monad.Trans
import Control.Concurrent
import Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import System.Environment
import Debug.Trace

--
--import Control.Monad.State
--import MFlow.Forms.Internals
(!>) = const -- flip trace

--test= runTest [(15,"shop")]

main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath ""
   addMessageFlows
       [(""    , transient $ runFlow  mainmenu)
       ,("shop", runFlow shopCart)]
   env <- getEnvironment
   let port = fromIntegral . read . fromMaybe "80" $ lookup "PORT" env
   wait $ run port waiMessageFlow


attr= fromString
text = fromString

data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes | PreventBack | Multicounter
            | Combination
            | FViewMonad | Counter | WDialog
            deriving (Bounded, Enum,Read, Show,Typeable)


mainmenu=   do
       setHeader stdheader
--       setTimeouts 100 0
       r <- ask $  do
              requires[CSSFile "http://jqueryui.com/resources/demos/style.css"]
              wcached "menu" 0 $
               b <<  "BASIC"
               ++>  br ++> wlink CountI       << b <<  "increase an Int"
               <|>  br ++> wlink CountS       << b <<  "increase a String"
               <|>  br ++> wlink Select       << b <<  "select options"
               <|>  br ++> wlink CheckBoxes   << b <<  "checkboxes"
               <|>  br ++> wlink Radio        << b <<  "Radio buttons"

               <++  br <>  br                 <> b <<  "WIDGET ACTIONS & CALLBACKS"
               <|>  br ++> wlink Action       << b <<  "Example of action, executed when a widget is validated"
               <|>  br ++> wlink FViewMonad   << b <<  "Flow in the View monad"
               <|>  br ++> wlink Counter      << b <<  "Counter"
               <|>  br ++> wlink Multicounter << b <<  "Multicounter"
               <|>  br ++> wlink Combination  << b <<  "combination of three active widgets"
               <|>  br ++> wlink WDialog      << b <<  "modal dialog"

               <++  br <>  br                 <> b <<  "DYNAMIC WIDGETS"
               <|>  br ++> wlink Ajax         << b <<  "AJAX example"
               <|>  br ++> wlink Autocomp     << b <<  "autocomplete"
               <|>  br ++> wlink AutocompList << b <<  "autocomplete List"
               <|>  br ++> wlink ListEdit     << b <<  "list edition"
               <|>  br ++> wlink Grid         << b <<  "grid"
               <|>  br ++> wlink TextEdit     << b <<  "Content Management"
               <++  br <>  br                 <> b <<  "STATEFUL PERSISTENT FLOW"
                 <> br <>  a ! href (attr "/shop") <<  "shopping"   -- ordinary Blaze.Html link

                 <> br <>  br <> b <<  "OTHERS"
               <|>  br ++> wlink Login        << b <<  "login/logout"
               <|>  br ++> wlink PreventBack  << b <<  "Prevent going back after a transaction"



       case r of
             CountI    ->  clickn  (0 :: Int)
             CountS    ->  clicks "1"
             Action    ->  actions 1
             Ajax      ->  ajaxsample
             Select    ->  options
             CheckBoxes -> checkBoxes
             TextEdit  ->  textEdit
             Grid      ->  grid
             Autocomp  ->  autocomplete1
             AutocompList -> autocompList
             ListEdit  ->  wlistEd
             Radio     ->  radio
             Login     ->  loginSample
             PreventBack -> preventBack
             Multicounter-> multicounter
             FViewMonad  -> sumInView
             Counter    -> counter
             Combination -> combination
             WDialog     -> wdialog1

wdialog1= do
     q <- ask $ pageFlow "diag" $ do
               r <-  wform $ p<< "please enter your name" ++> getString (Just "your name") <** submitButton "ok"
               wdialog "({modal: true})" "question" . wform $
                   p << ("Do your name is \""++r++"\"?") ++> getBool True "yes" "no" <** submitButton "ok"

     liftIO $ print q
     if q then ask $ wlink () << b << "thanks"
          else wdialog1

sumInView= ask $ p << "ask for three numbers in the same page and display the result.\
                      \It is possible to modify the inputs and the sum will reflect it"
               ++> sumWidget
               **> wlink () << text "exit"

formWidget=  wform $ do
      (n,s) <- (,) <$> p << "Who are you?"
                   ++> getString Nothing <! hint "name"     <++ br
                   <*> getString Nothing <! hint "surname"  <++ br
                   <** submitButton "ok" <++ br

      flag <- b << "Do you " ++> getRadio[radiob "work?",radiob "study?"] <++ br

      r<-case flag of
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
onClickSubmit= [("onclick","if(window.jQuery){\n\
                                  \$(this).parent().submit();}\n\
                           \else {this.form.submit()}")]
radiob s n= wlabel (text s) $ setRadio s n <! onClickSubmit

sumWidget= do
      n1 <- p << "Enter first number"  ++> getInt Nothing <** submitButton "enter" <++ br
      n2 <- p << "Enter second number" ++> getInt Nothing <** submitButton "enter" <++ br
      n3 <- p << "Enter third number"  ++> getInt Nothing <** submitButton "enter" <++ br
      p <<  ("The result is: "++show (n1 + n2 + n3))  ++>  wlink () << b << " menu"
      <++ p << "you can change them to see the result"



combination = ask $
     p << "Three active widgets in the same page with autoRefresh. Each widget refresh itself \
          \with Ajax. If Ajax is not active, they will refresh by sending a new page."
     ++> hr
     ++> p << "Login widget (use admin/admin)" ++> autoRefresh (pageFlow "r" wlogin)  <++ hr
     **> p << "Counter widget" ++> autoRefresh (pageFlow "c" (counterWidget 0))  <++ hr
     **> p << "Dynamic form widget" ++> autoRefresh (pageFlow "f" formWidget) <++ hr
     **> wlink () << b << "exit"

wlogin :: View Html IO ()
wlogin= wform (do
    username <- getCurrentUser
    if username /= anonymous
     then return username
     else do
      name <- getString Nothing <! hint "username" <++ br
      pass <- getPassword <! focus <** submitButton "login" <++ br
      val  <- userValidate (name,pass)
      case val of
        Just msg -> notValid msg
        Nothing  -> login name >> return name)

   `wcallback` (\name -> b << ("logged as " ++ name)
                     ++> p << ("navigate away of this page before logging out")
                     ++>  wlink "logout"  << b << " logout")
   `wcallback`  const (logout >>  wlogin)

focus = [("onload","this.focus()")]


multicounter= do
 let explain= p << "This example emulates the"
              <> a ! href (attr "http://www.seaside.st/about/examples/multicounter?_k=yBJEDEGp")
                    << " seaside example"
              <> p << "It uses various copies of the " <> a ! href (attr "/noscript/counter") << "counter widget "
              <> text "instantiated in the same page. This is an example of how it is possible to "
              <> text "compose widgets with independent behaviours"

 ask $ explain ++> add (counterWidget 0) [1,2] <|> wlink () << p << "exit"


add widget list= firstOf [pageFlow (show i) widget <++ hr | i <-list]

counter1= do
    ask $ wlink "p" <<p<<"press here"
    ask $ pageFlow "c" $ ex  [ wlink i << text (show (i :: Int)) | i <- [1..] ]
               where

               ex (a:as)= a >> ex as
counter= do
   let explain= p <<"This example emulates the"
                <> a ! href (attr "http://www.seaside.st/about/examples/counter") << "seaside counter example"
                <> p << "This widget uses a callback to permit an independent"
                <> p << "execution flow for each widget." <> a ! href (attr "/noscript/multicounter") << "Multicounter" <> (text " instantiate various counter widgets")
                <> p << "But while the seaside case the callback update the widget object, in this case"
                <> p << "the callback call generates a new copy of the counter with the value modified."

   ask $ explain ++> pageFlow "c" (counterWidget 0) <++ br <|> wlink () << p << "exit"

counterWidget n= do
  (h2 << show n !> show n
   ++> wlink "i" << b << " ++ "
   <|> wlink "d" << b << " -- ")
  `wcallback`
    \op -> case op  of
      "i" -> counterWidget (n + 1)    !> "increment"
      "d" -> counterWidget (n - 1)    !> "decrement"

rpaid= unsafePerformIO $ newMVar (0 :: Int)


preventBack= do
    ask $ wlink () << b << "press here to pay 100000 $ "
    payIt
    paid  <- liftIO $ readMVar rpaid
    preventGoingBack . ask $   p << "You already paid 100000 before"
                           ++> p << "you can no go back until the end of the buy process"
                           ++> wlink () << p << "Please press here to continue"
    ask $   p << ("you paid "++ show paid)
        ++> wlink () << p << "Press here to go to the menu or press the back button to verify that you can not pay again"
    where
    payIt= liftIO $ do
      print "paying"
      paid <- takeMVar  rpaid
      putMVar rpaid $ paid + 100000

options= do
   r <- ask $ getSelect (setSelectedOption ("" :: String) (p <<  "select a option") <|>
                         setOption "red"  (b <<  "red")     <|>
                         setSelectedOption "blue" (b <<  "blue")    <|>
                         setOption "Green"  (b <<  "Green")  )
                         <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p <<  " menu")

   breturn()
   where
   dosummit= [("onchange","this.form.submit()")]

checkBoxes= do
   r <- ask $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                           <> (setCheckBox False "Green" <++ b <<  "green")
                           <> (setCheckBox False "blue"  <++ b <<  "blue"))
              <** submitButton "submit"


   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")
   breturn()

autocomplete1= do
   r <- ask $   p <<  "Autocomplete "
            ++> p <<  "when su press submit, the box value  is returned"
            ++> wautocomplete Nothing filter1 <! hint "red,green or blue"
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")
   breturn()
   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

autocompList= do
   r <- ask $   p <<  "Autocomplete with a list of selected entries"
            ++> p <<  "enter  and press enter"
            ++> p <<  "when su press submit, the entries are returned"
            ++> wautocompleteList "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")
   breturn()
   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

grid = do
  let row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 0)
                          <*> tdborder <<< getString (Just "")
                          <++ tdborder << delLink)
      addLink= a ! href (attr "#")
                 ! At.id (attr "wEditListAdd")
                 <<  "add"
      delLink= a ! href (attr "#")
                 ! onclick (attr "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode)")
                 <<  "delete"
      tdborder= td ! At.style  (attr "border: solid 1px")

  r <- ask $ addLink ++> ( wEditList table  row ["",""] "wEditListAdd") <** submitButton "submit"
  ask $   p << (show r ++ " returned")
      ++> wlink () (p <<  " back to menu")

  breturn()

wlistEd= do
   r <-  ask  $   addLink
              ++> br
              ++> (wEditList El.div getString1   ["hi", "how are you"] "wEditListAdd")
              <++ br
              <** submitButton "send"

   ask $   p << (show r ++ " returned")
       ++> wlink () (p <<  " back to menu")
   breturn()

   where
   addLink = a ! At.id  (attr "wEditListAdd")
               ! href (attr "#")
               $ b << "add"
   delBox  =  input ! type_   (attr "checkbox")
                    ! checked (attr "")
                    ! onclick (attr "this.parentNode.parentNode.removeChild(this.parentNode)")
   getString1 mx= El.div  <<< delBox ++> getString  mx <++ br


clickn n= do
   r <- ask $   p << b <<  "increase an Int"
            ++> wlink ("menu" :: String) (p <<  "menu")
            |+|  getInt (Just n)  <* submitButton "submit"
   case r of
    (Just _,_) -> ask $ wlink () << p << "thanks"
    (_, Just n') -> clickn $ n'+1

   breturn()
clicks s= do
   s' <- ask $  p << b <<  "increase a String"
             ++> p << b <<  "press the back button to go back to the menu"
             ++>(getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just (b << "length must be < 5") else Nothing )
   clicks $ s'++ "1"
   breturn()

radio = do
   r <- ask $    p << b <<  "Radio buttons"
             ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")
   breturn()


ajaxsample= do
   r <- ask $   p << b <<  "Ajax example that increment the value in a box"
            ++> do
                 let elemval= "document.getElementById('text1').value"
                 ajaxc <- ajax $ \n -> return . B.pack $ elemval <> "='" <> show(read  n +1) <>  "'"
                 b <<  "click the box "
                   ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc  elemval)] <** submitButton "submit"
   ask $ p << ( show r ++ " returned")  ++> wlink () (p <<  " menu")
   breturn()

---- recursive action
--actions n=do
--  ask $ wlink () (p <<  "exit from action")
--     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )


actions n= do
  r<- ask $   p << b <<  "Two  boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  ask $ p << ( show r ++ " returned")  ++> wlink () (p <<  " menu")
  where
  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum, Show,Read , Typeable)

newtype Cart= Cart (V.Vector Int) deriving Typeable
emptyCart= Cart $ V.fromList [0,0,0]

shopCart  = do

   setHeader $ \html -> p << ( El.span <<
     "A persistent flow  (uses step). The process is killed after 100 seconds of inactivity \
     \but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n \
     \Defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.\n\
     \The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events."

     <> html)
   setTimeouts 100 (60 * 60)
   shopCart1
   where
   shopCart1 =  do
     o <-  step . ask $ do
             let moreexplain= p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"
             Cart cart <- getSessionData `onNothing` return emptyCart

             moreexplain
              ++>
              (table ! At.style (attr "border:1;width:20%;margin-left:auto;margin-right:auto")
              <<< caption <<  "choose an item"
              ++> thead << tr << ( th << b <<   "item" <> th << b <<  "times chosen")
              ++> (tbody
                  <<< tr ! rowspan (attr "2") << td << linkHome
                  ++> (tr <<< td <<< wlink  IPhone (b <<  "iphone") <++  td << ( b <<  show ( cart V.! 0))
                  <|>  tr <<< td <<< wlink  IPod   (b <<  "ipod")   <++  td << ( b <<  show ( cart V.! 1))
                  <|>  tr <<< td <<< wlink  IPad   (b <<  "ipad")   <++  td << ( b <<  show ( cart V.! 2)))
                  <++  tr << td <<  linkHome
                  ))
     let i =fromEnum o
     Cart cart <- getSessionData `onNothing` return emptyCart
     setSessionData . Cart $ cart V.// [(i, cart V.!  i + 1 )]
     shopCart1

    where
    linkHome= a ! href  (attr $ "/" ++ noScript) << b <<  "home"


loginSample= do
    ask $ p <<  "Please login with admin/admin"
            ++> userWidget (Just "admin") userLogin
    user <- getCurrentUser
    ask $ b <<  ("user logged as " <>  user) ++> wlink () (p <<  " logout and go to menu")
    logout



textEdit= do
    let first=  p << i <<
                   (El.span <<  "this is a page with"
                   <> b <<  " two " <> El.span <<  "paragraphs. this is the first")

        second= p << i <<  "This is the original  of the second paragraph"



    ask $   p << b <<  "An example of content management"
        ++> first
        ++> second
        ++> wlink () (p <<  "click here to edit it")


    ask $   p <<  "Please login with admin/admin to edit it"
        ++> userWidget (Just "admin") userLogin

    ask $   p <<  "Now you can click the fields and edit them"
        ++> p << b <<  "to save an edited field, double click on it"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink () (p <<  "click here to see it as a normal user")

    logout

    ask $   p <<  "the user sees the edited content. He can not edit"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink () (p <<  "click to continue")

    ask $   p <<  "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << "End of edit field demo" ++> wlink () (p <<  "click here to go to menu")



stdheader= html . body

stdheader1 c= docTypeHtml  $ body $
      a ! At.style (attr "-align:center") ! href ( attr  "/html/MFlow/index.html") << h1 <<  "MFlow"
   <> br
   <> hr
   <> (El.div ! At.style (attr "position:fixed;top:40px;left:0%\
                         \;width:50%\
                         \;margin-left:10px;margin-right:10px") $
          h2 <<  "Example of some features."
--       <> h3 <<  "This demo uses warp and blaze-html"

       <> br <> c)
   <> (El.div ! At.style (attr "position:fixed;top:40px;left:50%;width:50%") $
          h2 <<  "Documentation"
       <> br
       <> p  << a ! href (attr "/html/MFlow/index.html") <<  "MFlow package description and documentation"
       <> p  << a ! href (attr "demos.blaze.hs") <<  "download demo source code"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/issues") <<  "bug tracker"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow") <<  "source repository"
       <> p  << a ! href (attr "http://hackage.haskell.org/package/MFlow") <<  "Hackage repository"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/11/mflow-now-widgets-can-express.html") <<  "MFlow: now the widgets can express requirements"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html") <<  "On the \"spirit\" of MFlow. Anatomy of a Widget"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html") <<  "MFlow active widgets example"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2013/01/stateful-but-stateless-at-last-thanks.html") <<  "Stateful, but virtually stateless, thanks to event sourcing"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/11/i-just-added-some-templatingcontent.html") <<  "Content Management and multilanguage in MFlow"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/10/testing-mflow-applications_9.html") <<  "Testing MFlow applications"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/09/a.html") <<  "A Web app. that creates Haskel computations from form responses, that store, retrieve and execute them? It´s easy"
       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/09/announce-mflow-015.html") <<  "ANNOUNCE MFlow 0.1.5 Web app server for stateful processes with safe, composable user interfaces."
       )
