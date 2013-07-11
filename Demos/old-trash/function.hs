{-# OPTIONS -XDeriveDataTypeable
            #-}
module Main where

import Data.Typeable
import MFlow.Wai.XHtml.All
import Control.Concurrent

main= do
 syncWrite SyncManual
 addMessageFlows [("",runFlow ops)]
 forkIO $ run 80 waiMessageFlow
 adminLoop


ops= do
  i <- ask $ p << ("Enter a number. This number will be the parameter for a function\n"
               ++ "that will be defined by menu if it has not been defined previously")
               ++> getInt Nothing

  f <- runFlowIn "fun" getf

  ask $ p << ("The result is: " ++ show (f i)) ++> wlink ()  (bold << "next")
  ops
  where
  getf  = do
    op <- step . ask $ p << "let define the function: which operation?"
                     ++> getSelect(
                          setOption Plus (bold << "+") <|>
                          setOption Times (bold << "*"))
                     <**  submitButton "submit"
    num <- step . ask $ p << "give me another number" ++> getInt Nothing

    return $ case op of
      Plus ->  (+ num)
      Times -> (* num)

data Ops= Plus | Times deriving (Read, Show, Typeable)
