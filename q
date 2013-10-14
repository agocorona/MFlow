[1mdiff --git a/Demos/Menu.hs b/Demos/Menu.hs[m
[1mindex 5045098..4cddaf3 100644[m
[1m--- a/Demos/Menu.hs[m
[1m+++ b/Demos/Menu.hs[m
[36m@@ -65,7 +65,7 @@[m [mdata Options= CountI | CountS | Radio[m
 [m
 absLink ref = wcached (show ref) 0 . wlink ref[m
 [m
[31m-noAutoRefresh= [("class","_noAutoRefresh")][m
[32m+[m
 mainMenu :: View Html IO Options[m
 mainMenu= autoRefresh $[m
   ul<<<(  li << b "About this menu" ++> article menuarticle[m
[1mdiff --git a/Demos/PushDecrease.hs b/Demos/PushDecrease.hs[m
[1mindex 539a05d..67697aa 100644[m
[1m--- a/Demos/PushDecrease.hs[m
[1m+++ b/Demos/PushDecrease.hs[m
[36m@@ -8,7 +8,7 @@[m [mimport Text.Hamlet[m
 import Control.Concurrent[m
 [m
 import Menu[m
[31m--- to run it alone comment import menu and uncomment:[m
[32m+[m[32m-- to run it alone, comment "import menu" and uncomment:[m
 --main= runNavigation "" $ transientNav pushDecrease[m
 [m
 [m
[36m@@ -30,9 +30,9 @@[m [mpushDecrease= do[m
 [m
  where[m
  counter tv = push Html 0 $ do[m
[31m-      setTimeouts 2 0     -- kill  the thread when count finish[m
[32m+[m[32m      setTimeouts 2 0     -- kill  the thread after 2 s of incactivity, when count finish[m
       n <- atomic $ readTVar tv[m
[31m-      if (n== 0)[m
[32m+[m[32m      if (n== -1)[m
         then  do[m
           script << "window.location='/'" ++> noWidget[m
         else do[m
[1mdiff --git a/Demos/TestREST.Loc.hs b/Demos/TestREST.Loc.hs[m
[1mdeleted file mode 100644[m
[1mindex d341296..0000000[m
[1m--- a/Demos/TestREST.Loc.hs[m
[1m+++ /dev/null[m
[36m@@ -1,36 +0,0 @@[m
[31m-{-# LINE 1 "INPUT" #-}[m
[31m-{-# OPTIONS -F -pgmF MonadLoc  #-}[m
[31m-module TestREST where[m
[31m-{-# LINE 3 "INPUT" #-}[m
[31m-import MFlow.Wai.Blaze.Html.All[m
[31m-{-# LINE 4 "INPUT" #-}[m
[31m-import Data.Monoid[m
[31m-{-# LINE 5 "INPUT" #-}[m
[31m-import Data.String[m
[31m-{-# LINE 6 "INPUT" #-}[m
[31m-import Control.Monad.Loc[m
[31m-{-# LINE 14 "INPUT" #-}[m
[31m-testREST[m
[31m-  = Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (14, 11)"[m
[31m-      (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (15, 3)" (setTimeouts 120 0)[m
[31m-          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (16, 3)" (liftIO $ print "start/restart")[m
[31m-          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (18, 3)" (setHeader header1)[m
[31m-          option <- Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (20, 3)" (page $ wlink "a" << p << "letters " <++ p << "or" <|> wlink "1" << p << "numbers")[m
[31m-          Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (23, 3)"[m
[31m-            (case option of[m
[31m-                 "1" -> Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (24, 12)"[m
[31m-                          (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (25, 11)" (page $ wlink "2" << cont "1")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (26, 11)" (page $ wlink "3" << cont "2")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (27, 11)" (page $ wlink "4" << cont "3")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (28, 11)" (page $ wlink () << "menu"))[m
[31m-                 "a" -> Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (30, 12)"[m
[31m-                          (do Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (31, 11)" (page $ wlink "b" << cont "a")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (32, 11)" (page $ wlink "c" << cont "b")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (33, 11)" (page $ wlink "d" << cont "c")[m
[31m-                              Control.Monad.Loc.withLoc "testREST, TestREST(INPUT): (34, 11)" (page $ wlink () << "menu"))))[m
[31m-{-# LINE 37 "INPUT" #-}[m
[31m-cont x = p << "page for" <> b << x <> p << "goto next page"[m
[31m-{-# LINE 41 "INPUT" #-}[m
[31m-header1 h = html << body (text h)[m
[31m-  where {-# LINE 43 "INPUT" #-}[m
[31m-        text h = a ! href (fromString "http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html") << "see this" <> hr <> h <> hr <> a ! href (fromString "/") << "main menu"[m
[1mdiff --git a/Demos/TraceSample.Loc.hs b/Demos/TraceSample.Loc.hs[m
[1mdeleted file mode 100644[m
[1mindex c823261..0000000[m
[1m--- a/Demos/TraceSample.Loc.hs[m
[1m+++ /dev/null[m
[36m@@ -1,14 +0,0 @@[m
[31m-{-# LINE 1 "INPUT" #-}[m
[31m-{-# OPTIONS -F -pgmF MonadLoc  #-}[m
[31m-module Demos.TraceSample (traceSample) where[m
[31m-{-# LINE 4 "INPUT" #-}[m
[31m-import MFlow.Wai.Blaze.Html.All[m
[31m-{-# LINE 5 "INPUT" #-}[m
[31m-import Control.Monad.Loc[m
[31m-{-# LINE 7 "INPUT" #-}[m
[31m-traceSample[m
[31m-  = Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (7, 14)"[m
[31m-      (do Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (8, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> wlink () << "pres here")[m
[31m-          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (16, 3)" (page $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)[m
[31m-          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (20, 3)" (page $ p << "The trace will appear after you press the link. press one of the options available at the bottom of the page" ++> br ++> wlink () << "press here")[m
[31m-          Control.Monad.Loc.withLoc "traceSample, Demos.TraceSample(INPUT): (23, 3)" (page $ error $ "this is the error"))[m
[1mdiff --git a/Demos/demos.blaze.hs b/Demos/demos.blaze.hs[m
[1mdeleted file mode 100644[m
[1mindex 745a242..0000000[m
[1m--- a/Demos/demos.blaze.hs[m
[1m+++ /dev/null[m
[36m@@ -1,507 +0,0 @@[m
[31m-{-# LANGUAGE  DeriveDataTypeable #-}[m
[31m-module Main where[m
[31m-import MFlow.Wai.Blaze.Html.All[m
[31m-import Text.Blaze.Html5 as El[m
[31m-import Text.Blaze.Html5.Attributes as At hiding (step)[m
[31m-import Data.String[m
[31m-import Data.List[m
[31m-import Data.TCache[m
[31m-import Data.Typeable[m
[31m-import Control.Monad.Trans[m
[31m-import Control.Concurrent[m
[31m-import Control.Exception as E[m
[31m-import qualified Data.ByteString.Lazy.Char8 as B[m
[31m-import qualified Data.Text as T[m
[31m-import qualified Data.Vector as V[m
[31m-import Data.Maybe[m
[31m-import Data.Monoid[m
[31m-import System.IO.Unsafe[m
[31m-import System.Environment[m
[31m-import Debug.Trace[m
[31m-[m
[31m---[m
[31m---import Control.Monad.State[m
[31m---import MFlow.Forms.Internals[m
[31m-(!>) = const -- flip trace[m
[31m-[m
[31m---test= runTest [(15,"shop")][m
[31m-[m
[31m-main= do[m
[31m-   setAdminUser "admin" "admin"[m
[31m-   syncWrite SyncManual[m
[31m-   setFilesPath ""[m
[31m-   addMessageFlows[m
[31m-       [(""    , transient $ runFlow  mainmenu)[m
[31m-       ,("shop", runFlow shopCart)][m
[31m-   env <- getEnvironment[m
[31m-   let port = fromIntegral . read . fromMaybe "80" $ lookup "PORT" env[m
[31m-   wait $ run port waiMessageFlow[m
[31m-[m
[31m-[m
[31m-attr= fromString[m
[31m-text = fromString[m
[31m-[m
[31m-data Options= CountI | CountS | Radio[m
[31m-            | Login | TextEdit |Grid | Autocomp | AutocompList[m
[31m-            | ListEdit |Shop | Action | Ajax | Select[m
[31m-            | CheckBoxes | PreventBack | Multicounter[m
[31m-            | Combination[m
[31m-            | FViewMonad | Counter | WDialog[m
[31m-            deriving (Bounded, Enum,Read, Show,Typeable)[m
[31m-[m
[31m-[m
[31m-mainmenu=   do[m
[31m-       setHeader stdheader[m
[31m-       setTimeouts 100 0[m
[31m-       r <- ask $  do[m
[31m-              requires[CSSFile "http://jqueryui.com/resources/demos/style.css"][m
[31m-              wcached "menu" 0 $[m
[31m-               b <<  "BASIC"[m
[31m-               ++>  br ++> wlink CountI       << b <<  "increase an Int"[m
[31m-               <|>  br ++> wlink CountS       << b <<  "increase a String"[m
[31m-               <|>  br ++> wlink Select       << b <<  "select options"[m
[31m-               <|>  br ++> wlink CheckBoxes   << b <<  "checkboxes"[m
[31m-               <|>  br ++> wlink Radio        << b <<  "Radio buttons"[m
[31m-[m
[31m-               <++  br <>  br                 <> b <<  "WIDGET ACTIONS & CALLBACKS"[m
[31m-               <|>  br ++> wlink Action       << b <<  "Example of action, executed when a widget is validated"[m
[31m-               <|>  br ++> wlink FViewMonad   << b <<  "in page flow: sum of three numbers"[m
[31m-               <|>  br ++> wlink Counter      << b <<  "Counter"[m
[31m-               <|>  br ++> wlink Multicounter << b <<  "Multicounter"[m
[31m-               <|>  br ++> wlink Combination  << b <<  "combination of three active widgets"[m
[31m-               <|>  br ++> wlink WDialog      << b <<  "modal dialog"[m
[31m-[m
[31m-               <++  br <>  br                 <> b <<  "DYNAMIC WIDGETS"[m
[31m-               <|>  br ++> wlink Ajax         << b <<  "AJAX example"[m
[31m-               <|>  br ++> wlink Autocomp     << b <<  "autocomplete"[m
[31m-               <|>  br ++> wlink AutocompList << b <<  "autocomplete List"[m
[31m-               <|>  br ++> wlink ListEdit     << b <<  "list edition"[m
[31m-               <|>  br ++> wlink Grid         << b <<  "grid"[m
[31m-               <|>  br ++> wlink TextEdit     << b <<  "Content Management"[m
[31m-               <++  br <>  br                 <> b <<  "STATEFUL PERSISTENT FLOW"[m
[31m-                 <> br <>  a ! href (attr "/shop") <<  "shopping"   -- ordinary Blaze.Html link[m
[31m-[m
[31m-                 <> br <>  br <> b <<  "OTHERS"[m
[31m-               <|>  br ++> wlink Login        << b <<  "login/logout"[m
[31m-               <|>  br ++> wlink PreventBack  << b <<  "Prevent going back after a transaction"[m
[31m-[m
[31m-[m
[31m-[m
[31m-       case r of[m
[31m-             CountI    ->  clickn  (0 :: Int)[m
[31m-             CountS    ->  clicks "1"[m
[31m-             Action    ->  actions 1[m
[31m-             Ajax      ->  ajaxsample[m
[31m-             Select    ->  options[m
[31m-             CheckBoxes -> checkBoxes[m
[31m-             TextEdit  ->  textEdit[m
[31m-             Grid      ->  grid[m
[31m-             Autocomp  ->  autocomplete1[m
[31m-             AutocompList -> autocompList[m
[31m-             ListEdit  ->  wlistEd[m
[31m-             Radio     ->  radio[m
[31m-             Login     ->  loginSample[m
[31m-             PreventBack -> preventBack[m
[31m-             Multicounter-> multicounter[m
[31m-             FViewMonad  -> sumInView[m
[31m-             Counter    -> counter[m
[31m-             Combination -> combination[m
[31m-             WDialog     -> wdialog1[m
[31m-[m
[31m-wdialog1= ask  wdialogw[m
[31m-[m
[31m-wdialogw= pageFlow "diag" $ do[m
[31m-   r <- wform $ p<< "please enter your name" ++> getString (Just "your name") <** submitButton "ok"[m
[31m-   wdialog "({modal: true})" "question"  $ [m
[31m-           p << ("Do your name is \""++r++"\"?") ++> getBool True "yes" "no" <** submitButton "ok"[m
[31m-[m
[31m-  `wcallback` \q -> if not q then wdialogw[m
[31m-                      else  wlink () << b << "thanks, press here to go to the menu"[m
[31m-[m
[31m-[m
[31m-sumInView= ask $ p << "ask for three numbers in the same page and display the result.\[m
[31m-                      \It is possible to modify the inputs and the sum will reflect it"[m
[31m-               ++> sumWidget[m
[31m-               **> wlink () << text "exit"[m
[31m-[m
[31m-formWidget=  wform $ do[m
[31m-      (n,s) <- (,) <$> p << "Who are you?"[m
[31m-                   ++> getString Nothing <! hint "name"     <++ br[m
[31m-                   <*> getString Nothing <! hint "surname"  <++ br[m
[31m-                   <** submitButton "ok" <++ br[m
[31m-[m
[31m-      flag <- b << "Do you " ++> getRadio[radiob "work?",radiob "study?"] <++ br[m
[31m-[m
[31m-      r<- case flag of[m
[31m-         "work?" -> pageFlow "l"[m
[31m-                     $ Left  <$> b << "do you enjoy your work? "[m
[31m-                             ++> getBool True "yes" "no"[m
[31m-                             <** submitButton "ok" <++ br[m
[31m-[m
[31m-         "study?"-> pageFlow "r"[m
[31m-                     $ Right <$> b << "do you study in "[m
[31m-                             ++> getRadio[radiob "University"[m
[31m-                                         ,radiob "High School"][m
[31m-      u <-  getCurrentUser[m
[31m-      p << ("You are "++n++" "++s)[m
[31m-        ++> p << ("And your user is: "++ u)[m
[31m-        ++> case r of[m
[31m-             Left fl ->   p << ("You work and it is " ++ show fl ++ " that you enjoy your work")[m
[31m-                            ++> noWidget[m
[31m-[m
[31m-             Right stu -> p << ("You study at the " ++ stu)[m
[31m-                            ++> noWidget[m
[31m-[m
[31m-[m
[31m-hint s= [("placeholder",s)][m
[31m-onClickSubmit= [("onclick","if(window.jQuery){\n\[m
[31m-                                  \$(this).parent().submit();}\n\[m
[31m-                           \else {this.form.submit()}")][m
[31m-radiob s n= wlabel (text s) $ setRadio s n <! onClickSubmit[m
[31m-[m
[31m-sumWidget= do[m
[31m-      n1 <- p << "Enter first number"  ++> getInt Nothing <** submitButton "enter" <++ br[m
[31m-      n2 <- p << "Enter second number" ++> getInt Nothing <** submitButton "enter" <++ br[m
[31m-      n3 <- p << "Enter third number"  ++> getInt Nothing <** submitButton "enter" <++ br[m
[31m-      p <<  ("The result is: "++show (n1 + n2 + n3))  ++>  wlink () << b << " menu"[m
[31m-      <++ p << "you can change them to see the result"[m
[31m-[m
[31m-[m
[31m-[m
[31m-combination = ask $[m
[31m-     p << "Three active widgets in the same page with autoRefresh. Each widget refresh itself \[m
[31m-          \with Ajax. If Ajax is not active, they will refresh by sending a new page."[m
[31m-     ++> hr[m
[31m-     ++> p << "Login widget (use admin/admin)" ++> autoRefresh (pageFlow "r" wlogin)  <++ hr[m
[31m-     **> p << "Counter widget" ++> autoRefresh (pageFlow "c" (counterWidget 0))  <++ hr[m
[31m-     **> p << "Dynamic form widget" ++> autoRefresh (pageFlow "f" formWidget) <++ hr[m
[31m-     **> wlink () << b << "exit"[m
[31m-[m
[31m-wlogin :: View Html IO ()[m
[31m-wlogin= wform (do[m
[31m-    username <- getCurrentUser[m
[31m-    if username /= anonymous[m
[31m-     then return username[m
[31m-     else do[m
[31m-      name <- getString Nothing <! hint "username" <++ br[m
[31m-      pass <- getPassword <! focus <** submitButton "login" <++ br[m
[31m-      val  <- userValidate (name,pass)[m
[31m-      case val of[m
[31m-        Just msg -> notValid msg[m
[31m-        Nothing  -> login name >> return name)[m
[31m-[m
[31m-   `wcallback` (\name -> b << ("logged as " ++ name)[m
[31m-                     ++> p << ("navigate away of this page before logging out")[m
[31m-                     ++>  wlink "logout"  << b << " logout")[m
[31m-   `wcallback`  const (logout >>  wlogin)[m
[31m-[m
[31m-focus = [("onload","this.focus()")][m
[31m-[m
[31m-[m
[31m-multicounter= do[m
[31m- let explain= p << "This example emulates the"[m
[31m-              <> a ! href (attr "http://www.seaside.st/about/examples/multicounter?_k=yBJEDEGp")[m
[31m-                    << " seaside example"[m
[31m-              <> p << "It uses various copies of the " <> a ! href (attr "/noscript/counter") << "counter widget "[m
[31m-              <> text "instantiated in the same page. This is an example of how it is possible to "[m
[31m-              <> text "compose widgets with independent behaviours"[m
[31m-[m
[31m- ask $ explain ++> add (counterWidget 0) [1,2] <|> wlink () << p << "exit"[m
[31m-[m
[31m-[m
[31m-add widget list= firstOf [pageFlow (show i) widget <++ hr | i <- list][m
[31m-[m
[31m-counter1= do[m
[31m-    ask $ wlink "p" <<p<<"press here"[m
[31m-    ask $ pageFlow "c" $ ex  [ wlink i << text (show (i :: Int)) | i <- [1..] ][m
[31m-               where[m
[31m-[m
[31m-               ex (a:as)= a >> ex as[m
[31m-counter= do[m
[31m-   let explain= p <<"This example emulates the"[m
[31m-                <> a ! href (attr "http://www.seaside.st/about/examples/counter") << "seaside counter example"[m
[31m-                <> p << "This widget uses a callback to permit an independent"[m
[31m-                <> p << "execution flow for each widget." <> a ! href (attr "/noscript/multicounter") << "Multicounter" <> (text " instantiate various counter widgets")[m
[31m-                <> p << "But while the seaside case the callback update the widget object, in this case"[m
[31m-                <> p << "the callback call generates a new copy of the counter with the value modified."[m
[31m-[m
[31m-   ask $ explain ++> pageFlow "c" (counterWidget 0) <++ br <|> wlink () << p << "exit"[m
[31m-[m
[31m-counterWidget n= do[m
[31m-  (h2 << show n !> show n[m
[31m-   ++> wlink "i" << b << " ++ "[m
[31m-   <|> wlink "d" << b << " -- ")[m
[31m-  `wcallback`[m
[31m-    \op -> case op  of[m
[31m-      "i" -> counterWidget (n + 1)    !> "increment"[m
[31m-      "d" -> counterWidget (n - 1)    !> "decrement"[m
[31m-[m
[31m-rpaid= unsafePerformIO $ newMVar (0 :: Int)[m
[31m-[m
[31m-[m
[31m-preventBack= do[m
[31m-    ask $ wlink () << b << "press here to pay 100000 $ "[m
[31m-    payIt[m
[31m-    paid  <- liftIO $ readMVar rpaid[m
[31m-    preventGoingBack . ask $   p << "You already paid 100000 before"[m
[31m-                           ++> p << "you can no go back until the end of the buy process"[m
[31m-                           ++> wlink () << p << "Please press here to continue"[m
[31m-    ask $   p << ("you paid "++ show paid)[m
[31m-        ++> wlink () << p << "Press here to go to the menu or press the back button to verify that you can not pay again"[m
[31m-    where[m
[31m-    payIt= liftIO $ do[m
[31m-      print "paying"[m
[31m-      paid <- takeMVar  rpaid[m
[31m-      putMVar rpaid $ paid + 100000[m
[31m-[m
[31m-options= do[m
[31m-   r <- ask $ getSelect (setSelectedOption ""  (p <<  "select a option") <|>[m
[31m-                         setOption "red"  (b <<  "red")     <|>[m
[31m-                         setSelectedOption "blue" (b <<  "blue")    <|>[m
[31m-                         setOption "Green"  (b <<  "Green")  )[m
[31m-                         <! dosummit[m
[31m-   ask $ p << (r ++ " selected") ++> wlink () (p <<  " menu")[m
[31m-[m
[31m-[m
[31m-   where[m
[31m-   dosummit= [("onchange","this.form.submit()")][m
[31m-[m
[31m-checkBoxes= do[m
[31m-   r <- ask $ getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")[m
[31m-                           <> (setCheckBox False "Green" <++ b <<  "green")[m
[31m-                           <> (setCheckBox False "blue"  <++ b <<  "blue"))[m
[31m-              <** submitButton "submit"[m
[31m-[m
[31m-[m
[31m-   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")[m
[31m-[m
[31m-[m
[31m-autocomplete1= do[m
[31m-   r <- ask $   p <<  "Autocomplete "[m
[31m-            ++> p <<  "when su press submit, the box value  is returned"[m
[31m-            ++> wautocomplete Nothing filter1 <! hint "red,green or blue"[m
[31m-            <** submitButton "submit"[m
[31m-   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")[m
[31m-[m
[31m-   where[m
[31m-   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"][m
[31m-[m
[31m-autocompList= do[m
[31m-   r <- ask $   p <<  "Autocomplete with a list of selected entries"[m
[31m-            ++> p <<  "enter  and press enter"[m
[31m-            ++> p <<  "when su press submit, the entries are returned"[m
[31m-            ++> wautocompleteList "red,green,blue" filter1 ["red"][m
[31m-            <** submitButton "submit"[m
[31m-   ask $ p << ( show r ++ " selected")  ++> wlink () (p <<  " menu")[m
[31m-[m
[31m-   where[m
[31m-   filter1 s = return $ filter (isPrefixOf s) ["red","reed rose","green","green grass","blue","blues"][m
[31m-[m
[31m-grid = do[m
[31m-  let row _= tr <<< ( (,) <$> tdborder <<< getInt (Just 0)[m
[31m-                          <*> tdborder <<< getString (Just "")[m
[31m-                          <++ tdborder << delLink)[m
[31m-      addLink= a ! href (attr "#")[m
[31m-                 ! At.id (attr "wEditListAdd")[m
[31m-                 <<  "add"[m
[31m-      delLink= a ! href (attr "#")[m
[31m-                 ! onclick (attr "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode)")[m
[31m-                 <<  "delete"[m
[31m-      tdborder= td ! At.style  (attr "border: solid 1px")[m
[31m-[m
[31m-  r <- ask $ addLink ++> ( wEditList table  row ["",""] "wEditListAdd") <** submitButton "submit"[m
[31m-  ask $   p << (show r ++ " returned")[m
[31m-      ++> wlink () (p <<  " back to menu")[m
[31m-[m
[31m-[m
[31m-[m
[31m-wlistEd= do[m
[31m-   r <-  ask  $   addLink[m
[31m-              ++> br[m
[31m-              ++> (wEditList El.div getString1   ["hi", "how are you"] "wEditListAdd")[m
[31m-              <++ br[m
[31m-              <** submitButton "send"[m
[31m-[m
[31m-   ask $   p << (show r ++ " returned")[m
[31m-       ++> wlink () (p <<  " back to menu")[m
[31m-[m
[31m-[m
[31m-   where[m
[31m-   addLink = a ! At.id  (attr "wEditListAdd")[m
[31m-               ! href (attr "#")[m
[31m-               $ b << "add"[m
[31m-   delBox  =  input ! type_   (attr "checkbox")[m
[31m-                    ! checked (attr "")[m
[31m-                    ! onclick (attr "this.parentNode.parentNode.removeChild(this.parentNode)")[m
[31m-   getString1 mx= El.div  <<< delBox ++> getString  mx <++ br[m
[31m-[m
[31m-[m
[31m-clickn n= do[m
[31m-   r <- ask $   p << b <<  "increase an Int"[m
[31m-            ++> wlink "menu"  << p <<  "menu"[m
[31m-            |+|  getInt (Just n)  <* submitButton "submit"[m
[31m-   case r of[m
[31m-    (Just _,_) -> return ()  --  ask $ wlink () << p << "thanks"[m
[31m-    (_, Just n') -> clickn $ n'+1[m
[31m-[m
[31m-[m
[31m-clicks s= do[m
[31m-   s' <- ask $  p << b <<  "increase a String"[m
[31m-             ++> p << b <<  "press the back button to go back to the menu"[m
[31m-             ++>(getString (Just s)[m
[31m-             <* submitButton "submit")[m
[31m-             `validate` (\s -> return $ if length s   > 5 then Just (b << "length must be < 5") else Nothing )[m
[31m-   clicks $ s'++ "1"[m
[31m-[m
[31m-[m
[31m-radio = do[m
[31m-   r <- ask $    p << b <<  "Radio buttons"[m
[31m-             ++> getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]][m
[31m-[m
[31m-   ask $ p << ( show r ++ " selected")  ++> wlink ()  << p <<  " menu"[m
[31m-[m
[31m-ajaxsample= do[m
[31m-   r <- ask $   p << b <<  "Ajax example that increment the value in a box"[m
[31m-            ++> do[m
[31m-                 let elemval= "document.getElementById('text1').value"[m
[31m-                 ajaxc <- ajax $ \n -> return . B.pack $ elemval <> "='" <> show(read  n +1) <>  "'"[m
[31m-                 b <<  "click the box "[m
[31m-                   ++> getInt (Just 0) <! [("id","text1"),("onclick", ajaxc  elemval)] <** submitButton "submit"[m
[31m-   ask $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"[m
[31m-[m
[31m-[m
[31m----- recursive action[m
[31m---actions n=do[m
[31m---  ask $ wlink () (p <<  "exit from action")[m
[31m---     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )[m
[31m-[m
[31m-[m
[31m-actions n= do[m
[31m-  r<- ask $   p << b <<  "Two  boxes with one action each one"[m
[31m-          ++> getString (Just "widget1") `waction` action[m
[31m-          <+> getString (Just "widget2") `waction` action[m
[31m-          <** submitButton "submit"[m
[31m-  ask $ p << ( show r ++ " returned")  ++> wlink ()  << p <<  " menu"[m
[31m-  where[m
[31m-  action n=  ask $ getString (Just $ n ++ " action")<** submitButton "submit action"[m
[31m-[m
[31m-data ShopOptions= IPhone | IPod | IPad deriving (Bounded, Enum, Show,Read , Typeable)[m
[31m-[m
[31m-newtype Cart= Cart (V.Vector Int) deriving Typeable[m
[31m-emptyCart= Cart $ V.fromList [0,0,0][m
[31m-[m
[31m-shopCart  = do[m
[31m-[m
[31m-   setHeader $ \html -> p << ( El.span <<[m
[31m-     "A persistent flow  (uses step). The process is killed after 100 seconds of inactivity \[m
[31m-     \but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n \[m
[31m-     \Defines a table with links enclosed that return ints and a link to the menu, that abandon this flow.\n\[m
[31m-     \The cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events."[m
[31m-[m
[31m-     <> html)[m
[31m-   setTimeouts 100 (60 * 60)[m
[31m-   shopCart1[m
[31m-   where[m
[31m-   shopCart1 =  do[m
[31m-     o <-  step . ask $ do[m
[31m-             let moreexplain= p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"[m
[31m-             Cart cart <- getSessionData `onNothing` return emptyCart[m
[31m-[m
[31m-             moreexplain[m
[31m-              ++>[m
[31m-              (table ! At.style (attr "border:1;width:20%;margin-left:auto;margin-right:auto")[m
[31m-              <<< caption <<  "choose an item"[m
[31m-              ++> thead << tr << ( th << b <<   "item" <> th << b <<  "times chosen")[m
[31m-              ++> (tbody[m
[31m-                  <<< tr ! rowspan (attr "2") << td << linkHome[m
[31m-                  ++> (tr <<< td <<< wlink  IPhone (b <<  "iphone") <++  td << ( b <<  show ( cart V.! 0))[m
[31m-                  <|>  tr <<< td <<< wlink  IPod   (b <<  "ipod")   <++  td << ( b <<  show ( cart V.! 1))[m
[31m-                  <|>  tr <<< td <<< wlink  IPad   (b <<  "ipad")   <++  td << ( b <<  show ( cart V.! 2)))[m
[31m-                  <++  tr << td <<  linkHome[m
[31m-                  ))[m
[31m-     let i =fromEnum o[m
[31m-     Cart cart <- getSessionData `onNothing` return emptyCart[m
[31m-     setSessionData . Cart $ cart V.// [(i, cart V.!  i + 1 )][m
[31m-     shopCart1[m
[31m-[m
[31m-    where[m
[31m-    linkHome= a ! href  (attr $ "/" ++ noScript) << b <<  "home"[m
[31m-[m
[31m-[m
[31m-loginSample= do[m
[31m-    ask $ p <<  "Please login with admin/admin"[m
[31m-            ++> userWidget (Just "admin") userLogin[m
[31m-    user <- getCurrentUser[m
[31m-    ask $ b <<  ("user logged as " <>  user) ++> wlink ()  << p <<  " logout and go to menu"[m
[31m-    logout[m
[31m-[m
[31m-[m
[31m-[m
[31m-textEdit= do[m
[31m-    let first=  p << i <<[m
[31m-                   (El.span <<  "this is a page with"[m
[31m-                   <> b <<  " two " <> El.span <<  "paragraphs. this is the first")[m
[31m-[m
[31m-        second= p << i <<  "This is the original  of the second paragraph"[m
[31m-[m
[31m-[m
[31m-[m
[31m-    ask $   p << b <<  "An example of content management"[m
[31m-        ++> first[m
[31m-        ++> second[m
[31m-        ++> wlink ()  << p <<  "click here to edit it"[m
[31m-[m
[31m-[m
[31m-    ask $   p <<  "Please login with admin/admin to edit it"[m
[31m-        ++> userWidget (Just "admin") userLogin[m
[31m-[m
[31m-    ask $   p <<  "Now you can click the fields and edit them"[m
[31m-        ++> p << b <<  "to save an edited field, double click on it"[m
[31m-        ++> tFieldEd "first"  first[m
[31m-        **> tFieldEd "second" second[m
[31m-        **> wlink ()  << p <<  "click here to see it as a normal user"[m
[31m-[m
[31m-    logout[m
[31m-[m
[31m-    ask $   p <<  "the user sees the edited content. He can not edit"[m
[31m-        ++> tFieldEd "first"  first[m
[31m-        **> tFieldEd "second" second[m
[31m-        **> wlink ()  << p <<  "click to continue"[m
[31m-[m
[31m-    ask $   p <<  "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key"[m
[31m-        ++> tField "first"[m
[31m-        **> tField "second"[m
[31m-        **> p << "End of edit field demo" ++> wlink ()  << p <<  "click here to go to menu"[m
[31m-[m
[31m-[m
[31m-[m
[31m-stdheader= html . body[m
[31m-[m
[31m-stdheader1 c= docTypeHtml  $ body $[m
[31m-      a ! At.style (attr "-align:center") ! href ( attr  "/html/MFlow/index.html") << h1 <<  "MFlow"[m
[31m-   <> br[m
[31m-   <> hr[m
[31m-   <> (El.div ! At.style (attr "position:fixed;top:40px;left:0%\[m
[31m-                         \;width:50%\[m
[31m-                         \;margin-left:10px;margin-right:10px") $[m
[31m-          h2 <<  "Example of some features."[m
[31m---       <> h3 <<  "This demo uses warp and blaze-html"[m
[31m-[m
[31m-       <> br <> c)[m
[31m-   <> (El.div ! At.style (attr "position:fixed;top:40px;left:50%;width:50%") $[m
[31m-          h2 <<  "Documentation"[m
[31m-       <> br[m
[31m-       <> p  << a ! href (attr "/html/MFlow/index.html") <<  "MFlow package description and documentation"[m
[31m-       <> p  << a ! href (attr "demos.blaze.hs") <<  "download demo source code"[m
[31m-       <> p  << a ! href (attr "https://github.com/agocorona/MFlow/issues") <<  "bug tracker"[m
[31m-       <> p  << a ! href (attr "https://github.com/agocorona/MFlow") <<  "source repository"[m
[31m-       <> p  << a ! href (attr "http://hackage.haskell.org/package/MFlow") <<  "Hackage repository"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/11/mflow-now-widgets-can-express.html") <<  "MFlow: now the widgets can express requirements"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/12/on-spirit-of-mflow-anatomy-of-widget.html") <<  "On the \"spirit\" of MFlow. Anatomy of a Widget"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2013/01/now-example-of-use-of-active-widget.html") <<  "MFlow active widgets example"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2013/01/stateful-but-stateless-at-last-thanks.html") <<  "Stateful, but virtually stateless, thanks to event sourcing"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/11/i-just-added-some-templatingcontent.html") <<  "Content Management and multilanguage in MFlow"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/10/testing-mflow-applications_9.html") <<  "Testing MFlow applications"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/09/a.html") <<  "A Web app. that creates Haskel computations from form responses, that store, retrieve and execute them? It´s easy"[m
[31m-       <> p  << a ! href (attr "http://haskell-web.blogspot.com.es/2012/09/announce-mflow-015.html") <<  "ANNOUNCE MFlow 0.1.5 Web app server for stateful processes with safe, composable user interfaces."[m
[31m-       )[m
[1mdiff --git a/Demos/readEvalLoop.hi b/Demos/readEvalLoop.hi[m
[1mdeleted file mode 100644[m
[1mindex b6e82fa..0000000[m
Binary files a/Demos/readEvalLoop.hi and /dev/null differ
[1mdiff --git a/Demos/readEvalLoop.o b/Demos/readEvalLoop.o[m
[1mdeleted file mode 100644[m
[1mindex 4ffaf39..0000000[m
Binary files a/Demos/readEvalLoop.o and /dev/null differ
[1mdiff --git a/Test/favicon.ico b/Test/favicon.ico[m
[1mdeleted file mode 100644[m
[1mindex e69de29..0000000[m
[1mdiff --git a/Test/testerror.hs b/Test/testerror.hs[m
[1mdeleted file mode 100644[m
[1mindex 208a97a..0000000[m
[1m--- a/Test/testerror.hs[m
[1m+++ /dev/null[m
[36m@@ -1,7 +0,0 @@[m
[31m-module Main where[m
[31m-import MFlow.Wai.Blaze.Html.All[m
[31m-[m
[31m-main= runNavigation "" . transientNav $ do[m
[31m-   page $ wlink () << "press"[m
[31m-   error "error"[m
[31m-[m
[1mdiff --git a/src/MFlow/Forms/Widgets.hs b/src/MFlow/Forms/Widgets.hs[m
[1mindex 13e5632..98603b2 100644[m
[1m--- a/src/MFlow/Forms/Widgets.hs[m
[1m+++ b/src/MFlow/Forms/Widgets.hs[m
[36m@@ -15,7 +15,7 @@[m [mto create other active widgets.[m
 [m
 module MFlow.Forms.Widgets ([m
 -- * Ajax refreshing of widgets[m
[31m-autoRefresh, appendUpdate, prependUpdate, push, UpdateMethod(..)[m
[32m+[m[32mautoRefresh, noAutoRefresh, appendUpdate, prependUpdate, push, UpdateMethod(..)[m
 [m
 -- * JQueryUi widgets[m
 ,datePicker, getSpinner, wautocomplete, wdialog,[m
[36m@@ -764,6 +764,31 @@[m [mautoRefresh[m
   -> View v m a[m
 autoRefresh w=  update "html" w[m
 [m
[32m+[m[32m-- | In some cases, it is neccessary that a link or form inside a 'autoRefresh' or 'update' block[m
[32m+[m[32m-- should not be autorefreshed, since it produces side effects in the rest of the page that[m
[32m+[m[32m-- affect to the rendering of the whole. If you like to refresh the whole page, simply add[m
[32m+[m[32m-- noAutoRefresh attribute to the widget to force the refresh of the whole page when it is activated.[m
[32m+[m[32m--[m
[32m+[m[32m-- That behaviour is common at the last sentence of the 'autoRefresh' block.[m
[32m+[m[32m--[m
[32m+[m[32m-- This is a cascade menu example.[m
[32m+[m[32m--[m
[32m+[m[32m-- > r <- page $ autoRefresh $ ul <<< do[m
[32m+[m[32m-- >        li <<< wlink OptionA << "option A"[m
[32m+[m[32m-- >        ul <<< (li <<< wlink OptionA1 << "Option A1" <! noAutoRefresh)[m
[32m+[m[32m-- >           <|> (li <<< wlink OptionA2 << "Option A2" <! noAutoRefresh)[m
[32m+[m[32m-- >        <|>...[m
[32m+[m[32m-- >           maybe other content[m
[32m+[m[32m-- >[m
[32m+[m[32m-- > case r of[m
[32m+[m[32m-- >    OptionA1 -> pageA1[m
[32m+[m[32m-- >    OptionA2 -> pageA2[m
[32m+[m[32m--[m
[32m+[m[32m-- when @option A@ is clicked, the two sub-options appear with autorefresh. Only the two[m
[32m+[m[32m-- lines are returned by the server using AJAX. but when Option A1-2 is pressed we want to[m
[32m+[m[32m-- present other pages, so we add the noAutorefresh attribute.[m
[32m+[m[32mnoAutoRefresh= [("class","_noAutoRefresh")][m
[32m+[m
 -- | does the same than autoRefresh but append the result of each request to the bottom of the widget[m
 appendUpdate  :: (MonadIO m,[m
      FormInput v)[m
[36m@@ -841,7 +866,7 @@[m [mupdate method w= do[m
 [m
   ajaxPostForm = "\nfunction ajaxPostForm(id) {\n\[m
     \var id1= $('#'+id);\n\[m
[31m-    \var idform= $('#'+id+' form');\n\[m
[32m+[m[32m    \var idform= $('#'+id+' form[class!=\"_noAutoRefresh\"]');\n\[m
     \idform.submit(function (event) {\n\[m
         \if (hadtimeout == true) return true;\n\[m
         \event.preventDefault();\n\[m
[1mdiff --git a/texts/ListEdit.hstop b/texts/ListEdit.hstop[m
[1mindex 5441377..c8ae4fc 100644[m
[1m--- a/texts/ListEdit.hstop[m
[1m+++ b/texts/ListEdit.hstop[m
[36m@@ -1 +1 @@[m
[31m-TField "ListEdit.hstop" "<h1>Edition of a list of widgets</h1><br><font face=\"helvetica\" size=\"2\">Example of a widget that edit, update and delete a list of user-defined widgets. In this case the widget is a text box with a delete checkbox attached&nbsp;</font><br>" [m
\ No newline at end of file[m
[32m+[m[32mTField "ListEdit.hstop" "<h1>Edition of a list of widgets</h1><br><font face=\"helvetica\" size=\"2\">Example of a widget that edit, update and delete a list of user-defined widgets. In this case the widget is a text box with a delete checkbox attached&nbsp;</font><br>"[m
\ No newline at end of file[m
[1mdiff --git a/texts/key2 b/texts/key2[m
[1mdeleted file mode 100644[m
[1mindex a9e0cdb..0000000[m
[1m--- a/texts/key2[m
[1m+++ /dev/null[m
[36m@@ -1 +0,0 @@[m
[31m-TField "key2" (Chunk "<p>what is your name?</p><p>this is a question</p><input type=\"text\" name=\"c0\" value=\"\">" Empty)[m
\ No newline at end of file[m
warning: LF will be replaced by CRLF in Demos/Menu.hs.
The file will have its original line endings in your working directory.
warning: LF will be replaced by CRLF in Demos/PushDecrease.hs.
The file will have its original line endings in your working directory.
warning: LF will be replaced by CRLF in src/MFlow/Forms/Widgets.hs.
The file will have its original line endings in your working directory.
