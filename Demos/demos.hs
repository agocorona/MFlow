{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main where
import MFlow.Wai.XHtml.All
import Data.TCache
import Control.Monad.Trans
import Data.Typeable
import Control.Concurrent
import Control.Exception as E
import qualified Data.ByteString.Char8 as SB

import Debug.Trace
(!>)= flip  trace


data Ops= Ints | Strings | Actions | Ajax deriving(Typeable,Read, Show)
main= do
   syncWrite SyncManual
   setFilesPath ""
   addFileServerWF
   forkIO $ run 80 $ waiMessageFlow  [("noscript",transient $ runFlow mainf)]
   adminLoop



stdheader c= p << "you can press the back button to go to the menu"+++ c
mainf=   do
       setHeader stdheader
       r <- ask $   wlink Ints (bold << "Ints") <|>
                    br ++> wlink Strings (bold << "Strings") <|>
                    br ++> wlink Actions (bold << "Example of a string widget with an action") <|>
                    br ++> wlink Ajax (bold << "Simple AJAX example")
       case r of
         Ints ->  clickn 0
         Strings ->  clicks "1"
         Actions ->  actions 1
         Ajax    ->  ajaxsample
       mainf

clickn (n :: Int)= do
   setHeader stdheader
   r <- ask $  wlink "menu" (p << "back")
           |+| getInt (Just n) <* submitButton "submit"
   case r of
    (Just _,_) -> breturn ()
    (_, Just n') -> clickn $ n'+1


clicks s= do
   setHeader stdheader
   s' <- ask $ (getString (Just s)
             <* submitButton "submit")
             `validate` (\s -> return $ if length s   > 5 then Just "length must be < 5" else Nothing )
   clicks $ s'++ "1"


ajaxheader html= thehtml << ajaxHead << p << "click the box" +++ html

ajaxsample= do
   setHeader ajaxheader
   ajaxc <- ajaxCommand    "document.getElementById('text1').value"
                           (\n ->  return $ "document.getElementById('text1').value='"++show(read  n +1)++"'")
   ask $ (getInt (Just 0) <! [("id","text1"),("onclick", ajaxc)])
   breturn()

actions n=do
  ask $ wlink () (p << "exit from action")
     <**((getInt (Just (n+1)) <** submitButton "submit" ) `waction` actions )
  breturn ()

