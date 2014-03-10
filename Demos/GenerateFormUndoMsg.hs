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
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,  ScopedTypeVariables, ExistentialQuantification #-}
module GenerateFormUndoMsg (
genFormUndoMsg
) where
import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Internals
import Control.Monad.State
import Data.Typeable
import Data.Monoid
import Prelude hiding (div)
import Text.Blaze.Html5.Attributes as At hiding (step,span,form)
import Data.List(nub)
import Control.Monad
import Data.List((\\))
import Debug.Trace
(!>)= flip trace

main=do
 userRegister "edituser" "edituser"
 runNavigation "nav" . step $ genFormUndoMsg


-- page with header
hpage w = page $ tField  "genFormUndoMsgHeader.html" **> w




genFormUndoMsg= do
    id <- getSessionId
    let title= "generateForm/"++id ++ "/form.html"
    initFormTemplate title

    desc <-  createForm 0 title

    when (null $ filter (== ShowResults) desc) $ setSessionData (desc ++ [ShowResults])

    hpage $ do
           wraw $ b "This is the form created. Test it"
           wraw $ hr
           generateForm title desc
           stop


    return()

type Template= String
data WType = Intv | Stringv | TextArea |OptionBox[String]
           | WCheckBoxes [String] | ShowResults
           | Form Template [WType] deriving (Typeable,Read,Show,Eq)

initFormTemplate title= do
  liftIO $ writetField (title ++ show 1) $
      p  "( delete this line. Press the save button to save the edits)"

  setSessionData ([] :: [WType])
  setSessionData $ Seq 0


data Result = forall a.(Typeable a, Show a) => Result a deriving (Typeable)

instance Show Result where
  show (Result x)= show x

genElem  Intv   =  Result <$> dField (getInt Nothing)
genElem  Stringv=  Result <$> dField (getString Nothing)
genElem TextArea=  Result <$> dField (getMultilineText  "")
genElem (OptionBox xs) =
    Result <$> getSelect (setSelectedOption ""(p   "select a option") <|>
               firstOf[setOption op  (fromStr  op) | op <- xs])

genElem (WCheckBoxes xs) =
    Result <$> getCheckBoxes(mconcat[setCheckBox False x <++ (fromStr x) | x <- xs])

genElem ShowResults = Result <$> do
-- w@FormElem f mr <- View $ runView $ do
      xs <- getSessionData `onNothing` return []
--  if null xs then  do
--      wdialog "{modal: true}" "Warning" $ wraw "you must insert at least another widget"
--      return ("failure" ::String)
--
--  else if not . null $ filter (== ShowResults) xs then do
--      wdialog "{modal: true}" "Warning" $ wraw "you already inserted this widget"
--      return "failure"
--
--  else  do
      pageFlow "" (allOf (map genElem (xs \\ [ShowResults]))) <|> return []
    `wcallback` (\r ->  pageFlow "button" $ do
      submitButton "submit" <|> return ""
      wraw $ h2 "Result:"
      dField(wraw $ b << show r)
      return "")
 case mr of
  Result ("failure" :: String) -> w >> stop
  _ ->

genElem (Form temp desc)= Result <$> generateForm temp desc

generateForm title xs=
   input ! At.type_ "hidden" ! name "p0" ! value "()"

   ++> (div ! At.id "p7"
   <<< input ! type_ "hidden" ! name "p0" ! value"()"
   ++> (template title $ witerate . pageFlow "" . allOf $ map genElem xs))


createForm n title= do
 desc    <- getSessionData `onNothing` return []
 Seq seq <- getSessionData `onNothing` return (Seq 0)

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

             content <- liftIO $ readtField  mempty (title ++ show n)
             fieldview <- generateView  wdesc seq
             setSessionData $ mappend desc [wdesc]
             liftIO . writetField (title ++ show (n+1)) $ content <> br <> fieldview
             return Nothing
           )
     <** divbody <<<  {- wform -} (edTemplate "edituser" (title ++ show n) (return ()) )
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
    FormElm render mr <- runView $ genElem desc
    n'' <- gets mfSequence
    setSessionData $ Seq n''
    return $ FormElm [] $ case mr of
      Just (Result x) -> do

      _ -> Just ( br <> br <> mconcat render :: Html)


nrlink x v= wlink x v <! noAutoRefresh

chooseWidget= autoRefresh $
       (p $ a ! At.class_ "_noAutoRefresh" ! At.href "/" $ "home/reset"  )

       ++>(p <<< do
              wlink ("text":: String)  "text field"
              ul <<<(li <<< nrlink Intv "returning Int"
                 <|> li <<< nrlink Stringv  "returning string"))

       <|> p <<< do nrlink TextArea "text area"

       <|> p <<< do
              wlink ("check" :: String)  "checkBoxes"
              ul <<<  getOptions "comb"

       <|> p <<< do
              wlink ("options" :: String)  "options"
              ul <<<  getOptions "opt"

       <|> p <<< nrlink ShowResults "Show Form Results"




getOptions pf = autoRefresh  $ do

        (op,_) <- (,)<$> getString Nothing <! [("size","8"),("placeholder","option")]
                     <*> submitButton "add"
                     <** submitButton "clear" `waction` const (delSessionData (undefined :: WType))

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

    **> do
        r   <- submitButton "create" <! noAutoRefresh
        ops <- getSessionData
        case ops of
            Nothing -> stop
            Just elem -> do
              delSessionData (undefined :: WType)
              return elem

--delParam par=  modify  $ \s -> s{mfEnv=filter ( (par /=) . fst) $ mfEnv s}
