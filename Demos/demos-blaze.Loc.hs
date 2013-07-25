{-# LINE 1 "INPUT" #-}
{-# OPTIONS -XDeriveDataTypeable -XQuasiQuotes   #-}
module Main where
{-# LINE 3 "INPUT" #-}
import MFlow.Wai.Blaze.Html.All
{-# LINE 4 "INPUT" #-}
import Text.Blaze.Html5 as El
{-# LINE 5 "INPUT" #-}
import Text.Blaze.Html5.Attributes as At hiding (step)
{-# LINE 6 "INPUT" #-}
import Data.List
{-# LINE 7 "INPUT" #-}
import Data.Typeable
{-# LINE 8 "INPUT" #-}
import Control.Monad.Trans
{-# LINE 10 "INPUT" #-}
import qualified Data.ByteString.Lazy.Char8 as B
{-# LINE 12 "INPUT" #-}
import qualified Data.Vector as V
{-# LINE 13 "INPUT" #-}
import Data.Maybe
{-# LINE 14 "INPUT" #-}
import Data.Monoid
{-# LINE 15 "INPUT" #-}
import System.IO.Unsafe
{-# LINE 16 "INPUT" #-}
import Debug.Trace
{-# LINE 17 "INPUT" #-}
import Data.String
{-# LINE 18 "INPUT" #-}
import Control.Concurrent.MVar
{-# LINE 19 "INPUT" #-}
import Control.Concurrent.STM
{-# LINE 20 "INPUT" #-}
import Control.Concurrent
{-# LINE 21 "INPUT" #-}
import Control.Monad
{-# LINE 23 "INPUT" #-}
import Text.Hamlet
{-# LINE 25 "INPUT" #-}
import TestREST
{-# LINE 27 "INPUT" #-}
import Control.Monad.Loc
{-# LINE 29 "INPUT" #-}
(!>) = const
{-# LINE 33 "INPUT" #-}
main
  = Control.Monad.Loc.withLoc "main, Main(INPUT): (33, 7)"
      (do Control.Monad.Loc.withLoc "main, Main(INPUT): (34, 4)" (setAdminUser "admin" "admin")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (35, 4)" (syncWrite SyncManual)
          Control.Monad.Loc.withLoc "main, Main(INPUT): (36, 4)" (setFilesPath "")
          Control.Monad.Loc.withLoc "main, Main(INPUT): (37, 4)" (addMessageFlows [("shop", runFlow shopCart), ("navigation", runFlow $ transientNav testREST)])
          Control.Monad.Loc.withLoc "main, Main(INPUT): (40, 4)" (runNavigation "" $ transientNav mainmenu))
{-# LINE 43 "INPUT" #-}
attr = fromString
{-# LINE 44 "INPUT" #-}
text = toMarkup
 
{-# LINE 46 "INPUT" #-}
data Options = CountI
             | CountS
             | Radio
             | Login
             | TextEdit
             | Grid
             | Autocomp
             | AutocompList
             | ListEdit
             | Shop
             | Action
             | Ajax
             | Select
             | CheckBoxes
             | PreventBack
             | Multicounter
             | Combination
             | FViewMonad
             | Counter
             | WDialog
             | Push
             | PushInc
             | Trace
             deriving (Bounded, Enum, Read, Show, Typeable)
{-# LINE 55 "INPUT" #-}
mainmenu
  = Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (55, 13)"
      (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (56, 8)" (setHeader stdheader)
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (58, 8)" (setTimeouts 100 0)
          r <- Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (59, 8)" (ask $ Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (59, 20)" (do Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (60, 15)" (wcached "menu" 0 $ b << "PUSH" ++> br ++> wlink Push << b << "Example of a widget with push" <|> br ++> wlink PushInc << b << "A push counter" <|> br ++> br ++> b << "ERROR TRACES" ++> br ++> wlink Trace << b << "Execution traces for errors" <|> br ++> br ++> b << "DIFFERENT KINDS OF FLOWS" ++> br ++> a ! href (attr "/navigation") << "REST navigation" ++> br ++> a ! href (attr "/shop") << "stateful flow: shopping" ++> br ++> br ++> b << "BASIC" ++> br ++> wlink CountI << b << "increase an Int" <|> br ++> wlink CountS << b << "increase a String" <|> br ++> wlink Select << b << "select options" <|> br ++> wlink CheckBoxes << b << "checkboxes" <|> br ++> wlink Radio << b << "Radio buttons" <++ br <> br <> b << "WIDGET ACTIONS & CALLBACKS" <|> br ++> wlink Action << b << "Example of action, executed when a widget is validated" <|> br ++> wlink FViewMonad << b << "in page flow: sum of three numbers" <|> br ++> wlink Counter << b << "Counter" <|> br ++> wlink Multicounter << b << "Multicounter" <|> br ++> wlink Combination << b << "combination of three active widgets" <|> br ++> wlink WDialog << b << "modal dialog" <|> br ++> br ++> b << "DYNAMIC WIDGETS" ++> br ++> wlink Ajax << b << "AJAX example" <|> br ++> wlink Autocomp << b << "autocomplete" <|> br ++> wlink AutocompList << b << "autocomplete List" <|> br ++> wlink ListEdit << b << "list edition" <|> br ++> wlink Grid << b << "grid" <|> br ++> wlink TextEdit << b << "Content Management" <|> br ++> br ++> b << "OTHERS" ++> br ++> wlink Login << b << "login/logout" <|> br ++> wlink PreventBack << b << "Prevent going back after a transaction")))
          Control.Monad.Loc.withLoc "mainmenu, Main(INPUT): (101, 8)"
            (case r of
                 CountI -> clickn (0 :: Int)
                 CountS -> clicks "1"
                 Action -> actions 1
                 Ajax -> ajaxsample
                 Select -> options
                 CheckBoxes -> checkBoxes
                 TextEdit -> textEdit
                 Grid -> grid
                 Autocomp -> autocomplete1
                 AutocompList -> autocompList
                 ListEdit -> wlistEd
                 Radio -> radio
                 Login -> loginSample
                 PreventBack -> preventBack
                 Multicounter -> multicounter
                 FViewMonad -> sumInView
                 Counter -> counter
                 Combination -> combination
                 WDialog -> wdialog1
                 Push -> pushSample
                 PushInc -> pushIncrease
                 Trace -> traceSample))
{-# LINE 130 "INPUT" #-}
wdialog1
  = Control.Monad.Loc.withLoc "wdialog1, Main(INPUT): (130, 11)"
      (do Control.Monad.Loc.withLoc "wdialog1, Main(INPUT): (131, 4)" (ask wdialogw)
          Control.Monad.Loc.withLoc "wdialog1, Main(INPUT): (132, 4)" (ask (wlink () << "out of the page flow, press here to go to the menu")))
{-# LINE 134 "INPUT" #-}
wdialogw
  = pageFlow "diag" $
      Control.Monad.Loc.withLoc "wdialogw, Main(INPUT): (134, 29)"
        (do r <- Control.Monad.Loc.withLoc "wdialogw, Main(INPUT): (135, 4)" (wform $ p << "please enter your name" ++> getString (Just "your name") <** submitButton "ok")
            Control.Monad.Loc.withLoc "wdialogw, Main(INPUT): (136, 4)" (wdialog "({modal: true})" "question" $ p << ("Do your name is \"" ++ r ++ "\"?") ++> getBool True "yes" "no" <** submitButton "ok"))
        `wcallback` \ q -> if not q then wdialogw else wlink () << b << "thanks, press here to exit from the page Flow"
{-# LINE 143 "INPUT" #-}
sumInView = ask $ p << "ask for three numbers in the same page and display the result.It is possible to modify the inputs and the sum will reflect it" ++> sumWidget
 
{-# LINE 147 "INPUT" #-}
formWidget :: View Html IO ()
{-# LINE 148 "INPUT" #-}
formWidget
  = Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (148, 15)"
      (do (n, s) <- Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (149, 7)" ((,) <$> p << "Who are you?" ++> getString Nothing <! hint "name" <++ br <*> getString Nothing <! hint "surname" <++ br <** submitButton "ok" <++ br)
          flag <- Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (154, 7)" (b << "Do you " ++> getRadio [radiob "work?", radiob "study?"] <++ br)
          r <- Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (156, 7)"
                 (case flag of
                      "work?" -> pageFlow "l" $ Left <$> b << "do you enjoy your work? " ++> getBool True "yes" "no" <** submitButton "ok" <++ br
                      "study?" -> pageFlow "r" $ Right <$> b << "do you study in " ++> getRadio [radiob "University", radiob "High School"])
          u <- Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (166, 7)" (getCurrentUser)
          Control.Monad.Loc.withLoc "formWidget, Main(INPUT): (167, 7)"
            (p << ("You are " ++ n ++ " " ++ s) ++> p << ("And your user is: " ++ u) ++>
               case r of
                   Left fl -> p << ("You work and it is " ++ show fl ++ " that you enjoy your work") ++> noWidget
                   Right stu -> p << ("You study at the " ++ stu) ++> noWidget))
{-# LINE 177 "INPUT" #-}
hint s = [("placeholder", s)]
{-# LINE 178 "INPUT" #-}
onClickSubmit = [("onclick", "if(window.jQuery){\n$(this).parent().submit();}\nelse {this.form.submit()}")]
{-# LINE 181 "INPUT" #-}
radiob s n = wlabel (text s) $ setRadio s n <! onClickSubmit
{-# LINE 183 "INPUT" #-}
sumWidget
  = pageFlow "sum" $
      Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (183, 30)"
        (do n <- Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (184, 7)"
                   (Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (184, 12)"
                      (do n1 <- Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (185, 12)" (p << "Enter first number" ++> getInt Nothing <++ br)
                          n2 <- Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (186, 12)" (p << "Enter second number" ++> getInt Nothing <++ br)
                          n3 <- Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (187, 12)" (p << "Enter third number" ++> getInt Nothing <++ br)
                          Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (188, 12)" (return (n1 + n2 + n3)))
                      <** br
                      ++> pageFlow "button" (submitButton "submit"))
            Control.Monad.Loc.withLoc "sumWidget, Main(INPUT): (193, 7)" (p << ("The result is: " ++ show n) ++> wlink () << b << " menu"))
        <++ p
        << "you can change the numbers in the boxes to see how the result changes"
{-# LINE 198 "INPUT" #-}
combination = ask $ Control.Monad.Loc.withLoc "combination, Main(INPUT): (198, 22)" (do Control.Monad.Loc.withLoc "combination, Main(INPUT): (199, 6)" (p << "Three active widgets in the same page with autoRefresh. Each widget refresh itself with Ajax. If Ajax is not active, they will refresh by sending a new page.")) ++> hr ++> p << "Login widget (use admin/admin)" ++> autoRefresh (pageFlow "r" wlogin) <++ hr **> p << "Counter widget" ++> autoRefresh (pageFlow "c" (counterWidget 0)) <++ hr **> p << "Dynamic form widget" ++> autoRefresh (pageFlow "f" formWidget) <++ hr **> wlink () << b << "exit"
 
{-# LINE 207 "INPUT" #-}
wlogin :: View Html IO ()
{-# LINE 208 "INPUT" #-}
wlogin
  = (Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (208, 11)"
       (do username <- Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (209, 5)" (getCurrentUser)
           Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (210, 5)"
             (if username /= anonymous then return username else
                Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (212, 11)"
                  (do name <- Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (213, 7)" (getString Nothing <! hint "username" <++ br)
                      pass <- Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (214, 7)" (getPassword <! focus <** submitButton "login" <++ br)
                      val <- Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (215, 7)" (userValidate (name, pass))
                      Control.Monad.Loc.withLoc "wlogin, Main(INPUT): (216, 7)"
                        (case val of
                             Just msg -> notValid msg
                             Nothing -> login name >> return name)))))
      `wcallback` (\ name -> b << ("logged as " ++ name) ++> p << ("navigate away of this page before logging out") ++> wlink "logout" << b << " logout")
      `wcallback` const (logout >> wlogin)
{-# LINE 225 "INPUT" #-}
focus = [("onload", "this.focus()")]
{-# LINE 228 "INPUT" #-}
multicounter
  = Control.Monad.Loc.withLoc "multicounter, Main(INPUT): (228, 15)"
      (do let {-# LINE 229 "INPUT" #-}
              explain = p << "This example emulates the" <> a ! href (attr "http://www.seaside.st/about/examples/multicounter?_k=yBJEDEGp") << " seaside example" <> p << "It uses various copies of the " <> a ! href (attr "/noscript/counter") << "counter widget " <> text "instantiated in the same page. This is an example of how it is possible to " <> text "compose widgets with independent behaviours"
          Control.Monad.Loc.withLoc "multicounter, Main(INPUT): (236, 2)" (ask $ explain ++> add (counterWidget 0) [1 .. 4] <|> wlink () << p << "exit"))
{-# LINE 239 "INPUT" #-}
add widget list = firstOf [autoRefresh $ pageFlow (show i) widget <++ hr | i <- list]
{-# LINE 241 "INPUT" #-}
counter1
  = Control.Monad.Loc.withLoc "counter1, Main(INPUT): (241, 11)"
      (do Control.Monad.Loc.withLoc "counter1, Main(INPUT): (242, 5)" (ask $ wlink "p" << p << "press here")
          Control.Monad.Loc.withLoc "counter1, Main(INPUT): (243, 5)" (ask $ pageFlow "c" $ ex [wlink i << text (show (i :: Int)) | i <- [1 ..]]))
  where {-# LINE 246 "INPUT" #-}
        ex (a : as) = a >> ex as
{-# LINE 247 "INPUT" #-}
counter
  = Control.Monad.Loc.withLoc "counter, Main(INPUT): (247, 10)"
      (do let {-# LINE 248 "INPUT" #-}
              explain = p << "This example emulates the" <> a ! href (attr "http://www.seaside.st/about/examples/counter") << "seaside counter example" <> p << "This widget uses a callback to permit an independent" <> p << "execution flow for each widget." <> a ! href (attr "/noscript/multicounter") << "Multicounter" <> (text " instantiate various counter widgets") <> p << "But while the seaside case the callback update the widget object, in this case" <> p << "the callback call generates a new copy of the counter with the value modified."
          Control.Monad.Loc.withLoc "counter, Main(INPUT): (255, 4)" (ask $ explain ++> pageFlow "c" (counterWidget 0) <++ br <|> wlink () << p << "exit"))
{-# LINE 257 "INPUT" #-}
counterWidget n
  = Control.Monad.Loc.withLoc "counterWidget, Main(INPUT): (257, 18)" (do Control.Monad.Loc.withLoc "counterWidget, Main(INPUT): (258, 3)" ((h2 << show n !> show n ++> wlink "i" << b << " ++ " <|> wlink "d" << b << " -- "))) `wcallback`
      \ op ->
        case op of
            "i" -> counterWidget (n + 1) !> "increment"
            "d" -> counterWidget (n - 1) !> "decrement"
{-# LINE 266 "INPUT" #-}
rpaid = unsafePerformIO $ newMVar (0 :: Int)
{-# LINE 269 "INPUT" #-}
preventBack
  = Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (269, 14)"
      (do Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (270, 5)" (ask $ wlink () << b << "press here to pay 100000 $ ")
          Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (271, 5)" (payIt)
          paid <- Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (272, 5)" (liftIO $ readMVar rpaid)
          Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (273, 5)" (preventGoingBack . ask $ p << "You already paid 100000 before" ++> p << "you can no go back until the end of the buy process" ++> wlink () << p << "Please press here to continue")
          Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (277, 5)" (ask $ p << ("you paid " ++ show paid) ++> wlink () << p << "Press here to go to the menu or press the back button to verify that you can not pay again"))
  where {-# LINE 280 "INPUT" #-}
        payIt
          = liftIO $
              Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (280, 21)"
                (do Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (281, 7)" (print "paying")
                    paid <- Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (282, 7)" (takeMVar rpaid)
                    Control.Monad.Loc.withLoc "preventBack, Main(INPUT): (283, 7)" (putMVar rpaid $ paid + 100000))
{-# LINE 285 "INPUT" #-}
options
  = Control.Monad.Loc.withLoc "options, Main(INPUT): (285, 10)"
      (do r <- Control.Monad.Loc.withLoc "options, Main(INPUT): (286, 4)" (ask $ getSelect (setSelectedOption "" (p << "select a option") <|> setOption "red" (b << "red") <|> setOption "blue" (b << "blue") <|> setOption "Green" (b << "Green")) <! dosummit)
          Control.Monad.Loc.withLoc "options, Main(INPUT): (291, 4)" (ask $ p << (r ++ " selected") ++> wlink () (p << " menu")))
  where {-# LINE 295 "INPUT" #-}
        dosummit = [("onchange", "this.form.submit()")]
{-# LINE 297 "INPUT" #-}
checkBoxes
  = Control.Monad.Loc.withLoc "checkBoxes, Main(INPUT): (297, 13)"
      (do r <- Control.Monad.Loc.withLoc "checkBoxes, Main(INPUT): (298, 4)" (ask $ getCheckBoxes ((setCheckBox False "Red" <++ b << "red") <> (setCheckBox False "Green" <++ b << "green") <> (setCheckBox False "blue" <++ b << "blue")) <** submitButton "submit")
          Control.Monad.Loc.withLoc "checkBoxes, Main(INPUT): (304, 4)" (ask $ p << (show r ++ " selected") ++> wlink () (p << " menu")))
{-# LINE 307 "INPUT" #-}
autocomplete1
  = Control.Monad.Loc.withLoc "autocomplete1, Main(INPUT): (307, 16)"
      (do r <- Control.Monad.Loc.withLoc "autocomplete1, Main(INPUT): (308, 4)" (ask $ p << "Autocomplete " ++> p << "when su press submit, the box value  is returned" ++> wautocomplete Nothing filter1 <! hint "red,green or blue" <** submitButton "submit")
          Control.Monad.Loc.withLoc "autocomplete1, Main(INPUT): (312, 4)" (ask $ p << (show r ++ " selected") ++> wlink () (p << " menu")))
  where {-# LINE 315 "INPUT" #-}
        filter1 s = return $ filter (isPrefixOf s) ["red", "reed rose", "green", "green grass", "blue", "blues"]
{-# LINE 317 "INPUT" #-}
autocompList
  = Control.Monad.Loc.withLoc "autocompList, Main(INPUT): (317, 15)"
      (do r <- Control.Monad.Loc.withLoc "autocompList, Main(INPUT): (318, 4)" (ask $ p << "Autocomplete with a list of selected entries" ++> p << "enter  and press enter" ++> p << "when su press submit, the entries are returned" ++> wautocompleteList "red,green,blue" filter1 ["red"] <** submitButton "submit")
          Control.Monad.Loc.withLoc "autocompList, Main(INPUT): (323, 4)" (ask $ p << (show r ++ " selected") ++> wlink () (p << " menu")))
  where {-# LINE 326 "INPUT" #-}
        filter1 s = return $ filter (isPrefixOf s) ["red", "reed rose", "green", "green grass", "blue", "blues"]
{-# LINE 328 "INPUT" #-}
grid
  = Control.Monad.Loc.withLoc "grid, Main(INPUT): (328, 8)"
      (do let {-# LINE 329 "INPUT" #-}
              row _ = tr <<< ((,) <$> tdborder <<< getInt (Just 0) <*> tdborder <<< getTextBox (Just "") <++ tdborder << delLink)
              {-# LINE 332 "INPUT" #-}
              addLink = a ! href (attr "#") ! At.id (attr "wEditListAdd") << "add"
              {-# LINE 335 "INPUT" #-}
              delLink = a ! href (attr "#") ! onclick (attr "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode)") << "delete"
              {-# LINE 338 "INPUT" #-}
              tdborder = td ! At.style (attr "border: solid 1px")
          r <- Control.Monad.Loc.withLoc "grid, Main(INPUT): (340, 3)" (ask $ addLink ++> (wEditList table row ["", ""] "wEditListAdd") <** submitButton "submit")
          Control.Monad.Loc.withLoc "grid, Main(INPUT): (341, 3)" (ask $ p << (show r ++ " returned") ++> wlink () (p << " back to menu")))
{-# LINE 346 "INPUT" #-}
wlistEd
  = Control.Monad.Loc.withLoc "wlistEd, Main(INPUT): (346, 10)"
      (do r <- Control.Monad.Loc.withLoc "wlistEd, Main(INPUT): (347, 4)" (ask $ addLink ++> br ++> (wEditList El.div getString1 ["hi", "how are you"] "wEditListAdd") <++ br <** submitButton "send")
          Control.Monad.Loc.withLoc "wlistEd, Main(INPUT): (353, 4)" (ask $ p << (show r ++ " returned") ++> wlink () (p << " back to menu")))
  where {-# LINE 358 "INPUT" #-}
        addLink = a ! At.id (attr "wEditListAdd") ! href (attr "#") $ b << "add"
        {-# LINE 361 "INPUT" #-}
        delBox = input ! type_ (attr "checkbox") ! checked (attr "") ! onclick (attr "this.parentNode.parentNode.removeChild(this.parentNode)")
        {-# LINE 364 "INPUT" #-}
        getString1 mx = El.div <<< delBox ++> getString mx <++ br
{-# LINE 367 "INPUT" #-}
clickn n
  = Control.Monad.Loc.withLoc "clickn, Main(INPUT): (367, 11)"
      (do r <- Control.Monad.Loc.withLoc "clickn, Main(INPUT): (368, 4)" (ask $ p << b << "increase an Int" ++> wlink "menu" << p << "menu" |+| getInt (Just n) <* submitButton "submit")
          Control.Monad.Loc.withLoc "clickn, Main(INPUT): (371, 4)"
            (case r of
                 (Just _, _) -> return ()
                 (_, Just n') -> clickn $ n' + 1))
{-# LINE 376 "INPUT" #-}
clicks s
  = Control.Monad.Loc.withLoc "clicks, Main(INPUT): (376, 11)"
      (do s' <- Control.Monad.Loc.withLoc "clicks, Main(INPUT): (377, 4)" (ask $ p << b << "increase a String" ++> p << b << "press the back button to go back to the menu" ++> (getString (Just s) <* submitButton "submit") `validate` (\ s -> return $ if length s > 5 then Just (b << "length must be < 5") else Nothing))
          Control.Monad.Loc.withLoc "clicks, Main(INPUT): (382, 4)" (clicks $ s' ++ "1"))
{-# LINE 385 "INPUT" #-}
radio
  = Control.Monad.Loc.withLoc "radio, Main(INPUT): (385, 9)"
      (do r <- Control.Monad.Loc.withLoc "radio, Main(INPUT): (386, 4)" (ask $ p << b << "Radio buttons" ++> getRadio [\ n -> fromStr v ++> setRadioActive v n | v <- ["red", "green", "blue"]])
          Control.Monad.Loc.withLoc "radio, Main(INPUT): (389, 4)" (ask $ p << (show r ++ " selected") ++> wlink () << p << " menu"))
{-# LINE 391 "INPUT" #-}
ajaxsample
  = Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (391, 13)"
      (do r <- Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (392, 4)"
                 (ask $
                    p << b << "Ajax example that increment the value in a box" ++>
                      Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (393, 17)"
                        (do let {-# LINE 394 "INPUT" #-}
                                elemval = "document.getElementById('text1').value"
                            ajaxc <- Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (395, 18)" (ajax $ \ n -> return . B.pack $ elemval <> "='" <> show (read n + 1) <> "'")
                            Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (396, 18)" (b << "click the box " ++> getInt (Just 0) <! [("id", "text1"), ("onclick", ajaxc elemval)] <** submitButton "submit")))
          Control.Monad.Loc.withLoc "ajaxsample, Main(INPUT): (398, 4)" (ask $ p << (show r ++ " returned") ++> wlink () << p << " menu"))
{-# LINE 407 "INPUT" #-}
actions n
  = Control.Monad.Loc.withLoc "actions, Main(INPUT): (407, 12)"
      (do r <- Control.Monad.Loc.withLoc "actions, Main(INPUT): (408, 3)" (ask $ p << b << "Two  boxes with one action each one" ++> getString (Just "widget1") `waction` action <+> getString (Just "widget2") `waction` action <** submitButton "submit")
          Control.Monad.Loc.withLoc "actions, Main(INPUT): (412, 3)" (ask $ p << (show r ++ " returned") ++> wlink () << p << " menu"))
  where {-# LINE 414 "INPUT" #-}
        action n = ask $ getString (Just $ n ++ " action") <** submitButton "submit action"
 
{-# LINE 416 "INPUT" #-}
data ShopOptions = IPhone
                 | IPod
                 | IPad
                 deriving (Bounded, Enum, Show, Read, Typeable)
 
{-# LINE 418 "INPUT" #-}
newtype Cart = Cart (V.Vector Int)
             deriving Typeable
{-# LINE 419 "INPUT" #-}
emptyCart = Cart $ V.fromList [0, 0, 0]
{-# LINE 421 "INPUT" #-}
shopCart
  = Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (421, 13)"
      (do Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (423, 4)" (setHeader $ \ html -> p << (El.span << "A persistent flow  (uses step). The process is killed after 100 seconds of inactivity but it is restarted automatically. Event If you restart the whole server, it remember the shopping cart\n\n Defines a table with links that return ints and a link to the menu, that abandon this flow.\nThe cart state is not stored, Only the history of events is saved. The cart is recreated by running the history of events." <> html))
          Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (430, 4)" (setTimeouts 100 (60 * 60))
          Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (431, 4)" (shopCart1))
  where {-# LINE 433 "INPUT" #-}
        shopCart1
          = Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (433, 17)"
              (do o <- Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (434, 6)"
                         (step . ask $
                            Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (434, 25)"
                              (do let {-# LINE 435 "INPUT" #-}
                                      moreexplain = p << "The second parameter of \"setTimeout\" is the time during which the cart is recorded"
                                  Cart cart <- Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (436, 14)" (getSessionData `onNothing` return emptyCart)
                                  Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (438, 14)" (moreexplain ++> (table ! At.style (attr "border:1;width:20%;margin-left:auto;margin-right:auto") <<< caption << "choose an item" ++> thead << tr << (th << b << "item" <> th << b << "times chosen") ++> (tbody <<< tr ! rowspan (attr "2") << td << linkHome ++> (tr <<< td <<< wlink IPhone (b << "iphone") <++ td << (b << show (cart V.! 0)) <|> tr <<< td <<< wlink IPod (b << "ipod") <++ td << (b << show (cart V.! 1)) <|> tr <<< td <<< wlink IPad (b << "ipad") <++ td << (b << show (cart V.! 2))) <++ tr << td << linkHome)))))
                  let {-# LINE 450 "INPUT" #-}
                      i = fromEnum o
                  Cart cart <- Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (451, 6)" (getSessionData `onNothing` return emptyCart)
                  Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (452, 6)" (setSessionData . Cart $ cart V.// [(i, cart V.! i + 1)])
                  Control.Monad.Loc.withLoc "shopCart, Main(INPUT): (453, 6)" (shopCart1))
          where {-# LINE 456 "INPUT" #-}
                linkHome = a ! href (attr $ "/" ++ noScript) << b << "home"
{-# LINE 459 "INPUT" #-}
loginSample
  = Control.Monad.Loc.withLoc "loginSample, Main(INPUT): (459, 14)"
      (do Control.Monad.Loc.withLoc "loginSample, Main(INPUT): (460, 5)" (ask $ p << "Please login with admin/admin" ++> userWidget (Just "admin") userLogin)
          user <- Control.Monad.Loc.withLoc "loginSample, Main(INPUT): (462, 5)" (getCurrentUser)
          Control.Monad.Loc.withLoc "loginSample, Main(INPUT): (463, 5)" (ask $ b << ("user logged as " <> user) ++> wlink () << p << " logout and go to menu")
          Control.Monad.Loc.withLoc "loginSample, Main(INPUT): (465, 5)" (logout))
{-# LINE 469 "INPUT" #-}
textEdit
  = Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (469, 11)"
      (do let {-# LINE 470 "INPUT" #-}
              first = p << i << (El.span << "this is a page with" <> b << " two " <> El.span << "paragraphs. this is the first")
              {-# LINE 474 "INPUT" #-}
              second = p << i << "This is the original  of the second paragraph"
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (478, 5)" (ask $ p << b << "An example of content management" ++> first ++> second ++> wlink () << p << "click here to edit it")
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (484, 5)" (ask $ p << "Please login with admin/admin to edit it" ++> userWidget (Just "admin") userLogin)
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (487, 5)" (ask $ p << "Now you can click the fields and edit them" ++> p << b << "to save an edited field, double click on it" ++> tFieldEd "first" first **> tFieldEd "second" second **> wlink () << p << "click here to see it as a normal user")
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (493, 5)" (logout)
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (495, 5)" (ask $ p << "the user sees the edited content. He can not edit" ++> tFieldEd "first" first **> tFieldEd "second" second **> wlink () << p << "click to continue")
          Control.Monad.Loc.withLoc "textEdit, Main(INPUT): (500, 5)" (ask $ p << "When texts are fixed,the edit facility and the original texts can be removed. The content is indexed by the field key" ++> tField "first" **> tField "second" **> p << "End of edit field demo" ++> wlink () << p << "click here to go to menu"))
{-# LINE 509 "INPUT" #-}
stdheader c = docTypeHtml $ El.head << (El.title << "MFlow examples" <> link ! rel (attr "stylesheet") ! type_ (attr "text/css") ! href (attr "http://jqueryui.com/resources/demos/style.css")) <> (body $ a ! At.style (attr "align:center;") ! href (attr "http://hackage.haskell.org/packages/archive/MFlow/0.3.0.1/doc/html/MFlow-Forms.html") << h1 << "MFlow" <> br <> hr <> (El.div ! At.style (attr "float:left;width:50%;margin-left:10px;margin-right:10px;overflow:auto;") $ h2 << "Example of some features." <> br <> c) <> (El.div ! At.style (attr "float:right;width:45%;overflow:auto;") $ h2 << "Documentation" <> br <> p << a ! href (attr "/html/MFlow/index.html") << "MFlow package description and documentation" <> p << a ! href (attr "https://github.com/agocorona/MFlow/blob/master/Demos/demos-blaze.hs") << "download demo source code" <> p << a ! href (attr "https://github.com/agocorona/MFlow/issues") << "bug tracker" <> p << a ! href (attr "https://github.com/agocorona/MFlow") << "source repository" <> p << a ! href (attr "http://hackage.haskell.org/package/MFlow") << "Hackage repository" <> [shamlet| <script type="text/javascript" src="http://output18.rssinclude.com/output?type=js&amp;id=727700&amp;hash=8aa6c224101cac4ca2a7bebd6e28a2d7"></script>|]))
{-# LINE 537 "INPUT" #-}
traceSample
  = Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (537, 14)"
      (do Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (538, 3)" (page $ h2 << "Error trace example" ++> p << "MFlow now produces execution traces in case of error by making use of the backtracking mechanism" ++> p << "It is more detailed than a call stack" ++> p << "this example has a deliberate error" ++> br ++> p << "You must be logged as admin to see the trace" ++> autoRefresh wlogin **> br ++> br ++> wlink () << h2 << "press here to continue to the error trace")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (549, 3)" (page $ p << "the trace will appear after you press the link. press one of the options available at the bottomm of the page" ++> br ++> wlink () << "press here")
          Control.Monad.Loc.withLoc "traceSample, Main(INPUT): (552, 3)" (page $ undefined))
{-# LINE 554 "INPUT" #-}
pushSample
  = Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (554, 14)"
      (do tv <- Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (555, 3)" (liftIO $ newTVarIO $ Just "The content will be appended here")
          Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (556, 3)" (page $ h2 << "Push example" ++> p << "The content of the text box will be appended to the push widget above." ++> p << "A push widget can have links and form fields." ++> p << "Since they are asynchronous the communucation must be trough mutable variables" ++> p << "The input box is configured with autoRefresh" ++> hr ++> pageFlow "push" (push Append 5000 (disp tv) <** input tv) **> br ++> br ++> wlink () << b << "exit"))
  where {-# LINE 570 "INPUT" #-}
        disp tv
          = Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (570, 12)"
              (do Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (571, 7)" (setTimeouts 100 0)
                  line <- Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (572, 7)" (tget tv)
                  Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (573, 7)" (liftIO $ when (line == "kill") $ myThreadId >>= killThread)
                  Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (574, 7)" (p << line ++> noWidget))
        {-# LINE 576 "INPUT" #-}
        input tv
          = autoRefresh $
              Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (576, 27)"
                (do line <- Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (577, 7)" (getString Nothing <** submitButton "Enter")
                    Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (578, 7)" (tput tv line))
        {-# LINE 581 "INPUT" #-}
        tput tv x = atomic $ writeTVar tv (Just x)
        {-# LINE 583 "INPUT" #-}
        tget tv
          = atomic $
              Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (583, 21)"
                (do mr <- Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (584, 7)" (readTVar tv)
                    Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (585, 7)"
                      (case mr of
                           Nothing -> retry
                           Just r -> Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (587, 20)"
                                       (do Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (588, 11)" (writeTVar tv Nothing)
                                           Control.Monad.Loc.withLoc "pushSample, Main(INPUT): (589, 11)" (return r))))
{-# LINE 591 "INPUT" #-}
atomic = liftIO . atomically
{-# LINE 594 "INPUT" #-}
pushIncrease
  = Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (594, 15)"
      (do tv <- Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (595, 2)" (liftIO $ newTVarIO 30)
          Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (596, 2)" (page $ [shamlet|
   <div>
       <h2> Maxwell Smart push counter
       <p> This example shows a counter
       <p> To avoid unnecessary load, the push process will be killed when reaching 0
       <p> The last push message will be an script that will redirect to the menu"
       <h3> This message will be autodestroyed within ..

  |] ++> (counter tv <++ b << "seconds")))
  where {-# LINE 612 "INPUT" #-}
        counter tv
          = push Html 0 $
              Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (612, 29)"
                (do Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (613, 7)" (setTimeouts 100 0)
                    n <- Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (614, 7)" (atomic $ readTVar tv)
                    Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (615, 7)"
                      (if (n == (-1)) then
                         Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (616, 14)"
                           (do Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (617, 11)" (notValid $ script << "window.location='/'")
                               Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (618, 11)" (liftIO $ myThreadId >>= killThread))
                         else
                         Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (619, 14)"
                           (do Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (620, 11)" (atomic $ writeTVar tv $ n - 1)
                               Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (621, 11)" (liftIO $ threadDelay 1000000)
                               Control.Monad.Loc.withLoc "pushIncrease, Main(INPUT): (622, 11)" (h1 << (show n) ++> noWidget))))
