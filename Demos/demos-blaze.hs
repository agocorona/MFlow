{-# OPTIONS  -XDeriveDataTypeable -XQuasiQuotes -F -pgmF MonadLoc #-}
module Main where
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.List
import Data.Typeable
import Control.Monad.Trans

import qualified Data.ByteString.Lazy.Char8 as B
--import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import Debug.Trace
import Data.String
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import Text.Hamlet

import TestREST

import Control.Monad.Loc

(!>) = const -- flip trace

--test= runTest [(15,"shop")]

main= do
   setAdminUser "admin" "admin"
   syncWrite SyncManual
   setFilesPath ""
   addMessageFlows  [("shop", runFlow shopCart)
                    ,("navigation", runFlow $ transientNav testREST)]

   runNavigation "" $  transientNav mainmenu


attr= fromString
text = toMarkup

data Options= CountI | CountS | Radio
            | Login | TextEdit |Grid | Autocomp | AutocompList
            | ListEdit |Shop | Action | Ajax | Select
            | CheckBoxes | PreventBack | Multicounter
            | Combination
            | FViewMonad | Counter | WDialog |Push |Trace
            deriving (Bounded, Enum,Read, Show,Typeable)


mainmenu=   do
       setHeader stdheader
       
       setTimeouts 100 0
       r <- ask $  do
              wcached "menu" 0 $
               b <<  "PUSH"
               ++> br ++> wlink Push << b << "Example of a widget with push"
               <|> br ++> br ++>
               b <<  "ERROR TRACES"
               ++> br ++> wlink Trace << b << "Execution traces for errors"
               <|> br ++> br ++>
               b <<  "DIFFERENT KINDS OF FLOWS"
               ++> br ++>  a ! href (attr "/navigation") <<  "REST navigation"   -- ordinary Blaze.Html link
               ++> br ++>  a ! href (attr "/shop") <<  "stateful flow: shopping"   -- ordinary Blaze.Html link
                       
               ++>  br ++> br ++> b <<  "BASIC"
               ++>  br ++> wlink CountI       << b <<  "increase an Int"
               <|>  br ++> wlink CountS       << b <<  "increase a String"
               <|>  br ++> wlink Select       << b <<  "select options"
               <|>  br ++> wlink CheckBoxes   << b <<  "checkboxes"
               <|>  br ++> wlink Radio        << b <<  "Radio buttons"

               <++  br <>  br                 <> b <<  "WIDGET ACTIONS & CALLBACKS"
               <|>  br ++> wlink Action       << b <<  "Example of action, executed when a widget is validated"
               <|>  br ++> wlink FViewMonad   << b <<  "in page flow: sum of three numbers"
               <|>  br ++> wlink Counter      << b <<  "Counter"
               <|>  br ++> wlink Multicounter << b <<  "Multicounter"
               <|>  br ++> wlink Combination  << b <<  "combination of three active widgets"
               <|>  br ++> wlink WDialog      << b <<  "modal dialog"

               <|>  br ++>  br               ++> b <<  "DYNAMIC WIDGETS"
               ++>  br ++> wlink Ajax         << b <<  "AJAX example"
               <|>  br ++> wlink Autocomp     << b <<  "autocomplete"
               <|>  br ++> wlink AutocompList << b <<  "autocomplete List"
               <|>  br ++> wlink ListEdit     << b <<  "list edition"
               <|>  br ++> wlink Grid         << b <<  "grid"
               <|>  br ++> wlink TextEdit     << b <<  "Content Management"

               <|>  br ++>  br ++> b <<  "OTHERS"
               ++>  br ++> wlink Login        << b <<  "login/logout"
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
             Push        -> pushSample
             Trace       -> traceSample

--withSource txt w= [shamlet|
--   <iframe src= />"$(window).scrollTop($('*:contains('" ++ txt ++"').offset().top);"
--
--   |}

wdialog1= do
   ask  wdialogw
   ask (wlink () << "out of the page flow, press here to go to the menu")

wdialogw= pageFlow "diag" $ do
   r <- wform $ p << "please enter your name" ++> getString (Just "your name") <** submitButton "ok"
   wdialog "({modal: true})" "question"  $ 
           p << ("Do your name is \""++r++"\"?") ++> getBool True "yes" "no" <** submitButton "ok"

  `wcallback` \q -> if not q then wdialogw
                      else  wlink () << b << "thanks, press here to exit from the page Flow"


sumInView= ask $ p << "ask for three numbers in the same page and display the result.\
                      \It is possible to modify the inputs and the sum will reflect it"
               ++> sumWidget

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
onClickSubmit= [("onclick","if(window.jQuery){\n\
                                  \$(this).parent().submit();}\n\
                           \else {this.form.submit()}")]
radiob s n= wlabel (text s) $ setRadio s n <! onClickSubmit

sumWidget=  pageFlow "sum" $ do
      n <- do
           n1 <- p << "Enter first number"  ++> getInt Nothing <++ br
           n2 <- p << "Enter second number" ++> getInt Nothing <++ br
           n3 <- p << "Enter third number"  ++> getInt Nothing <++ br
           return (n1+ n2 + n3)

          -- factoring out the button
          <**  br ++> pageFlow "button" (submitButton "submit")
           
      p <<  ("The result is: "++show n)  ++>  wlink () << b << " menu"
      <++ p << "you can change the numbers in the boxes to see how the result changes"



combination =  ask $ do
     p << "Three active widgets in the same page with autoRefresh. Each widget refresh itself \
          \with Ajax. If Ajax is not active, they will refresh by sending a new page."
     ++> hr
     ++> p << "Login widget (use admin/admin)" ++> autoRefresh (pageFlow "r" wlogin)  <++ hr
     **> p << "Counter widget" ++> autoRefresh (pageFlow "c" (counterWidget 0))  <++ hr
     **> p << "Dynamic form widget" ++> autoRefresh (pageFlow "f" formWidget) <++ hr
     **> wlink () << b << "exit"

wlogin :: View Html IO ()
wlogin=  (do
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

 ask $ explain ++> add (counterWidget 0) [1..4] <|> wlink () << p << "exit"


add widget list= firstOf [autoRefresh $ pageFlow (show i) widget <++ hr | i <- list]

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
      "i" -> counterWidget (n + 1)                        !> "increment"
      "d" -> counterWidget (n - 1)                        !> "decrement"

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
   r <- ask $ getSelect (setSelectedOption ""  (p <<  "select a option") <|>
                         setOption "red"  (b <<  "red")                  <|>
                         setOption "blue" (b <<  "blue")                 <|>
                         setOption "Green"  (b <<  "Green")  )
                         <! dosummit
   ask $ p << (r ++ " selected") ++> wlink () (p <<  " menu")


   where
   dosummit= [("onchange","this.form.submit()")]

checkBoxes= do
   r <- ask $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                           <> (setCheckBox False "Green" <++ b <<  "green")
                           <> (setCheckBox False "blue"  <++ b <<  "blue"))
              <** submitButton "submit"


   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")


autocomplete1= do
   r <- ask $   p <<  "Autocomplete "
            ++> p <<  "when su press submit, the box value  is returned"
            ++> wautocomplete Nothing filter1 <! hint "red,green or blue"
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

autocompList= do
   r <- ask $   p <<  "Autocomplete with a list of selected entries"
            ++> p <<  "enter  and press enter"
            ++> p <<  "when su press submit, the entries are returned"
            ++> wautocompleteList "red,green,blue" filter1 ["red"]
            <** submitButton "submit"
   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")

   where
   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"]

grid = do
  let row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 0)
                          <*> tdborder <<< getTextBox (Just "")
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



wlistEd= do
   r <-  ask  $   addLink
              ++> br
              ++> (wEditList El.div getString1   ["hi", "how are you"] "wEditListAdd")
              <++ br
              <** submitButton "send"

   ask $   p << (show r ++ " returned")
       ++> wlink () (p <<  " back to menu")


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
            ++> wlink "menu"  << p <<  "menu"
            |+|  getInt (Just n)  <* submitButton "submit"
   case r of
    (Just _,_) -> return ()  --  ask $ wlink () << p << "thanks"
    (_, Just n') -> clickn $ n'+1


clicks s= do
   s' <- ask $  p << b <<  "increase a String"
             ++> p << b <<  "press the back button to go back to the menu"
             ++>(getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just (b << "length must be < 5") else Nothing )
   clicks $ s'++ "1"


radio = do
   r <- ask $    p << b <<  "Radio buttons"
             ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

   ask $ p << ( show r ++ " selected")  ++> wlink ()  << p <<  " menu"

ajaxsample= do
   r <- ask $   p << b <<  "Ajax example that increment the value in a box"
            ++> do
                 let elemval= "document.getElementById('text1').value"
                 ajaxc <- ajax $ \n -> return . B.pack $ elemval <> "='" <> show(read  n +1) <>  "'"
                 b <<  "click the box "
                   ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc  elemval)] <** submitButton "submit"
   ask $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"


---- recursive action
--actions n=do
--  ask $ wlink () (p <<  "exit from action")
--     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )


actions n= do
  r<- ask $   p << b <<  "Two  boxes with one action each one"
          ++> getString (Just "widget1") `waction` action
          <+> getString (Just "widget2") `waction` action
          <** submitButton "submit"
  ask $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"
  where
  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"

data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum, Show,Read , Typeable)

newtype Cart= Cart (V.Vector Int) deriving Typeable
emptyCart= Cart $ V.fromList [0,0,0]

shopCart  = do

   setHeader $ \html -> p << ( El.span <<
     "A persistent flow  (uses step). The process is killed after 100 seconds of inactivity \
     \but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n \
     \Defines a table with links that return ints and a link to the menu, that abandon this flow.\n\
     \The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events."

     <> html)
   setTimeouts 100 (60 * 60)
   shopCart1
   where
   shopCart1 =  do
     o <-  step . ask $ do
             let moreexplain= p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"
             Cart cart <- getSessionData `onNothing` return  emptyCart

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
    ask $   b <<  ("user logged as " <>  user)
        ++> wlink ()  << p <<  " logout and go to menu"
    logout



textEdit= do
    let first=  p << i <<
                   (El.span <<  "this is a page with"
                   <> b <<  " two " <> El.span <<  "paragraphs. this is the first")

        second= p << i <<  "This is the original  of the second paragraph"



    ask $   p << b <<  "An example of content management"
        ++> first
        ++> second
        ++> wlink ()  << p <<  "click here to edit it"


    ask $   p <<  "Please login with admin/admin to edit it"
        ++> userWidget (Just "admin") userLogin

    ask $   p <<  "Now you can click the fields and edit them"
        ++> p << b <<  "to save an edited field, double click on it"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink ()  << p <<  "click here to see it as a normal user"

    logout

    ask $   p <<  "the user sees the edited content. He can not edit"
        ++> tFieldEd "first"  first
        **> tFieldEd "second" second
        **> wlink ()  << p <<  "click to continue"

    ask $   p <<  "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"
        ++> tField "first"
        **> tField "second"
        **> p << "End of edit field demo" ++> wlink ()  << p <<  "click here to go to menu"



--stdheader= html . body

stdheader c= docTypeHtml
   $ El.head << (El.title << "MFlow examples"
     <> link ! rel ( attr "stylesheet")
             ! type_ ( attr "text/css")
             ! href (attr "http://jqueryui.com/resources/demos/style.css"))
   <> (body $ 
      a ! At.style (attr "align:center;") ! href ( attr  "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html") << h1 <<  "MFlow"
   <> br
   <> hr
   <> (El.div ! At.style (attr "float:left\
                         \;width:50%\
                         \;margin-left:10px;margin-right:10px;overflow:auto;") $
          h2 <<  "Example of some features."
--       <> h3 <<  "This demo uses warp and blaze-html"

       <> br <> c)
   <> (El.div ! At.style (attr "float:right;width:45%;overflow:auto;") $
          h2 <<  "Documentation"
       <> br
       <> p  << a ! href (attr "/html/MFlow/index.html") <<  "MFlow package description and documentation"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos/demos-blaze.hs") <<  "download demo source code"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/issues") <<  "bug tracker"
       <> p  << a ! href (attr "https://github.com/agocorona/MFlow") <<  "source repository"
       <> p  << a ! href (attr "http://hackage.haskell.org/package/MFlow") <<  "Hackage repository"
       <> [shamlet| <script type="text/javascript" src="http://output18.rssinclude.com/output?type=js&amp;id=727700&amp;hash=8aa6c224101cac4ca2a7bebd6e28a2d7"></script>|]

              ))

traceSample= do
  page $   h2 << "Error trace example"
       ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism"
       ++> p << "It is more detailed than a call stack"
       ++> p << "this example has a deliberate error"
       ++> br
       ++> p << "You must be logged as admin to see the trace"
       ++> autoRefresh wlogin
       **> br
       ++> br
       ++> wlink () << h2 << "press here to continue to the error trace"

  page $ p << "the trace will appear after you press the link. press one of the options available at the bottomm of the page"
           ++> br
           ++> wlink () << "press here"
  page $ undefined

pushSample=  do
  tv <- liftIO $ newTVarIO $ Just "The content will be appended here"
  page $   h2 << "push example"
       ++> p << "The content of the text box will be appended to the push widget above."
       ++> p << "A push widget can have links and form fields."
       ++> p << "Since they are asynchronous the communucation must be trough mutable variables"
       ++> p << "The input box is configured with autoRefresh"
       ++> hr

       ++> pageFlow "push" (push Append (disp tv) <** input tv)
       **> br
       ++> br
       ++> wlink () << b << "exit"

  where
  -- the widget being pushed:
  disp tv= do
      setTimeouts 100 0
      line <- tget tv
      liftIO $ when (line == "kill") $ myThreadId >>= killThread
      p <<  line ++> noWidget

  input tv= autoRefresh $ do
      line <- getString Nothing <** submitButton "Enter"
      tput tv line


  tput tv x = atomic $ writeTVar  tv ( Just x)

  tget tv= atomic $ do
      mr <- readTVar tv
      case mr of
         Nothing -> retry
         Just r -> do
          writeTVar tv Nothing
          return r

atomic= liftIO . atomically

