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
module GenerateFormUndo (
genFormUndo
) where
import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Internals
import Control.Monad.State
import Data.Typeable
import Data.Monoid
import Prelude hiding (div)
import Text.Blaze.Html5.Attributes as At hiding (step,span)
import Data.List(nub)
import Control.Monad


main=do
 userRegister "edituser" "edituser"
 runNavigation "nav" . step $ genFormUndo


-- page with header
hpage w = page $ tFieldEd "editor"  "genFormUndoHeader.html" "header" **> w


genFormUndo= do
    id <- getSessionId
    let title= id++"form.html"
    initFormTemplate title

    desc <-  createForm 0 title

    r <- hpage $ b "This is the form created, asking for input"
           ++> hr
           ++> generateForm title desc
           <++ br
           <** pageFlow "button" (submitButton "submit")

    hpage $  h3 "results of the form:" ++> p << show r ++> noWidget
    return()

type Template= String
data WType = Intv | Stringv | TextArea |OptionBox[String]
           | WCheckBoxes [String] | Form Template [WType] deriving (Typeable,Read,Show)

initFormTemplate title= do
  liftIO $ writetField (title ++ show 1) $
      p  "( delete this line. Press the save button to save the edits)"

  setSessionData ([] :: [WType])
  setSessionData $ Seq 0


data Result = forall a.(Typeable a, Show a) => Result a deriving (Typeable)

instance Show Result where
  show (Result x)= show x

genElem  Intv= Result <$> getInt Nothing
genElem  Stringv=  Result <$> getString Nothing
genElem TextArea=  Result <$> getMultilineText  ""
genElem (OptionBox xs) =
    Result <$> getSelect (setSelectedOption ""(p   "select a option") <|>
               firstOf[setOption op  (fromStr  op) | op <- xs])

genElem (WCheckBoxes xs) =
    Result <$> getCheckBoxes(mconcat[setCheckBox False x <++ (fromStr x) | x <- xs])

genElem (Form temp desc)= Result <$> generateForm temp desc

generateForm title xs=
   input ! At.type_ "hidden" ! name "p0" ! value "()"
   ++>  template title
   (pageFlow "" $ allOf $ map genElem xs )


createForm n title= do
 desc <- getSessionData `onNothing` return []
 Seq seq <-getSessionData `onNothing` return (Seq 0)

 r <- hpage $ do
    divmenu <<<(wlogin
       **> do
             br ++> wlink ("save" :: String) << b  "Save the form and continue"
                <++ br <> "(when finished)"
             content <- liftIO $ readtField  (mempty :: Html) (title ++ show n)
             liftIO . writetField title $ content
             liftIO $ forM_ [1 .. n] $ \n -> writetField (title ++ show n)  ("" :: Html)
             Just <$> getSessionData `onNothing` return []
       <|> do
             wdesc <- chooseWidget <++ hr
             setSessionData $ mappend desc [wdesc]
             content <- liftIO $ readtField  mempty (title ++ show n)
             fieldview <- generateView  wdesc seq
             liftIO . writetField (title ++ show (n+1)) $ content <> br <> fieldview
             return Nothing
           )
     <** divbody <<<  wform (edTemplate "edituser" (title ++ show n) (return ()) )
 case r of
   Just desc -> return desc
   Nothing -> createForm (n+1) title

divbody= div ! At.style "float:right;width:65%"
divmenu= div ! At.style "background-color:#EEEEEE;float:left;margin-left:10px;margin-right:10px;overflow:auto;"


newtype Seq= Seq Int deriving (Typeable)

generateView desc n= View $ do
    s <- get
    let n'= if n== 0 then 1 else n
    put s{mfSequence= n'}
    FormElm render _ <- runView $ genElem desc
    n'' <- gets mfSequence
    setSessionData $ Seq n''
    return $ FormElm [] $ Just ( br <> br <> mconcat render :: Html)



chooseWidget=
       (p $ a ! At.href "/" $ "home/reset") ++>

       (p <<< do wlink ("text":: String)  "text field"
                 ul <<<(li <<< wlink Intv "returning Int"
                    <|> li <<< wlink Stringv  "returning string"))

       <|> p <<< do wlink TextArea "text area"

       <|> p <<< do
              wlink ("check" :: String)  "checkBoxes"
              ul <<<  getOptions "comb"

       <|> p <<< do
              wlink ("options" :: String)  "options"
              ul <<<  getOptions "opt"






getOptions pf =
     do
      r <- wform $ submitButton "create" <|> submitButton "clear"

      case r of
        "create" -> do
          ops <- getSessionData
          case ops of
            Nothing -> stop
            Just elem -> return elem
        "clear" -> do
           delSessionData (undefined :: WType)
           stop

    <** do
        op <- wform $ getString Nothing <! [("size","8")
                                   ,("placeholder","option")]
                       <** submitButton "add" <++ br

        mops <- getSessionData
        ops' <- case (mops,pf) of
           (Nothing, "comb") -> do setSessionData $ WCheckBoxes [op] ; return [op]
           (Nothing, "opt")  -> do setSessionData $ OptionBox [op] ; return [op]
           (Just (OptionBox _), "comb") -> do setSessionData $ WCheckBoxes [op] ; return [op]
           (Just (WCheckBoxes _),"opt") -> do setSessionData $ OptionBox [op] ; return [op]
           (Just (WCheckBoxes ops),"comb") -> do
               let ops'= nub $ op:ops
               setSessionData . WCheckBoxes $ ops'
               return ops'
           (Just (OptionBox ops),"opt") ->  do
               let ops'= nub $ op:ops
               setSessionData . OptionBox $ ops'
               return ops'
        wraw $ mconcat [p << op | op <- ops']




--delParam par=  modify  $ \s -> s{mfEnv=filter ( (par /=) . fst) $ mfEnv s}
