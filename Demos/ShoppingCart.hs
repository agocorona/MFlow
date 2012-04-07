{-# OPTIONS -XDeriveDataTypeable -XScopedTypeVariables #-}
module Test where


import Control.MessageFlow.UserDefs
import Control.HackMessageFlow
import Hack(Env)
import Hack.Handler.SimpleServer

import qualified Data.Vector as V



main= do
   putStrLn $ options messageFlows
   run   80   $ hackMessageFlow messageFlows


options msgs= "in the browser choose\n\n" ++
     concat [ "http://machine/"++ i ++ "\n" | (i,_) <- msgs]

messageFlows=  [("shopping",   runFlow shopCart )]


hello :: Env -> IO String
hello env =  return  "hello, this is a stateless response"


--shopCart1 :: V.Vector Int -> FlowM (Workflow IO) b
shopCart  = do
   setTimeouts 10 20
   shopCart1 (V.fromList [0,0,0])
   where
   shopCart1 cart=  do
     i <- step $ ask (Selection{
           stitle ="choose an item" ,
           sheader= [ "item"   , "times chosen"],
           sbody= [(["iphone" , show $ cart V.! 0],0 :: Int),
                    (["ipad"   , show $ cart V.! 1],1),
                    (["ipod"   , show $ cart V.! 2],2)]})


     let newCart= cart V.// [(i, cart V.! i + 1 )]
     shopCart1 newCart

