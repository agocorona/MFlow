-----------------------------------------------------------------------------
--
-- Module      :  GenerateForm
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ExistentialQuantification #-}
module GenerateForm (

) where
import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Internals
import Control.Monad.State
import Unsafe.Coerce
import Data.Typeable
import Data.Monoid
import Data.String
import Prelude hiding (div)
import Text.Blaze.Html5.Attributes as At hiding (step)

--import Debug.Trace
--
--(!>)= flip trace

main=do
 userRegister "edituser" "edituser"
 runNavigation "nav" . step $ do

    let title= "form.html"

    initFormTemplate title

    desc <-  ask $ createForm title

    r <- ask $ b "This is the form created asking for input"
           ++> hr
           ++> generateForm title desc
           <++ br
           <** pageFlow "button" (submitButton "submit")

    ask $  h3 "results of the form:" ++> p << show r ++> noWidget
    return()

data WType = Intv | Stringv | TextArea |OptionBox[String]
           | CheckBoxes [String] deriving (Typeable,Read,Show)

initFormTemplate title= do
  liftIO $ writetField title $
      p  "( delete thiss line. Press the save button to save the edits)"

  setSessionData ([] :: [WType])

data Result = forall a.(Typeable a, Show a) => Result a deriving (Typeable)

instance Show Result where
  show (Result x)= show x

genElem  Intv= Result <$> getInt Nothing
genElem  Stringv=  Result <$> getString Nothing
genElem TextArea=  Result <$> getMultilineText (fromString "")
genElem (OptionBox xs) =
    Result <$> getSelect (setSelectedOption ""(p   "select a option") <|>
               firstOf[setOption op  (b <<  op) | op <- xs])

genElem (CheckBoxes xs) =
    Result <$> getCheckBoxes(firstOf[setCheckBox False x <++ (b << x) | x <- xs])


generateForm title xs=
           input ! At.type_ "hidden" ! name "p0" ! value "()"
           ++> template title
           (pageFlow "" $ allOf $ map genElem xs )

allOf xs= manyOf xs `validate` \rs ->
      if length rs== length xs
         then return Nothing
         else return $ Just mempty



createForm  title= do
 wraw $ h3 "Create a form"
 wraw $ h4 "1- login as edituser/edituser, 2- choose form elements, 3- edit the template \
           \4- save the template, 5- Save the form"
 divmenu <<<  (pageFlow "login" wlogin
  **> do p "when finished,"
            ++> wlink ("save" :: String) << b  "save the form and continue"
         getSessionData `onNothing` return []
  <** do
       wdesc <- chooseWidget <++ hr
       desc <- getSessionData `onNothing` return []
       setSessionData $ desc ++ [wdesc]
       content <- liftIO $ readtField  mempty title
       fieldview <- generateView  wdesc
       liftIO . writetField title $ content <> br <> fieldview
       )
 <**  divbody <<<  edTemplate "edituser" title (return ())

divbody= div ! At.style "float:right;width:65%"
divmenu= div ! At.style "background-color:#EEEEEE;float:left\
                 \;margin-left:10px;margin-right:10px;overflow:auto;"


newtype Seq= Seq Int deriving (Typeable)

generateView desc= View $ do
    Seq n <- getSessionData `onNothing` return (Seq 0)
    s <- get
    let n'= if n== 0 then 1 else n
    put s{mfSequence= n'}
    FormElm render _ <- runView $ genElem desc
    n'' <- gets mfSequence
    setSessionData $ Seq n''

    return $ FormElm [] $ Just ( mconcat render :: Html)



chooseWidget=
       p <<< do wlink ("text":: String)  "text field"
                ul <<<(li <<< wlink Intv "returning Int"
                   <|> li <<< wlink Stringv  "returning string")
       <|> p <<< do wlink TextArea "text area"
       <|> p <<< do
              wlink  ("options" :: String)  "options"
              ul <<< (OptionBox <$> getOptions "opt" [])


       <|> p <<< do
              wlink ("check" :: String)  "checkBoxes"
              ul <<< (CheckBoxes <$> getOptions "comb" [])


newtype Options= Options [String] deriving Typeable

getOptions pf ops=  pageFlow pf  $

     do wlink ("enter" ::String) << p  " create"
        getOptions

    <** do
        ops <- getOptions
        op <- getString Nothing <! [("size","8"),("placeholder","option")]
               <** submitButton "add" <++ br
        let ops'= op:ops
        setSessionData . Options $ ops'
        wraw $ mconcat [p << op | op <- ops']


    <** do
        wlink ("del" :: String) << p "delete options"
        setSessionData $ Options []

   where
   getOptions= do
     Options ops <-  getSessionData `onNothing`  return (Options [])
     return ops

