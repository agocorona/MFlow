{-# LANGUAGE   MultiParamTypeClasses, FlexibleInstances
             , UndecidableInstances, FunctionalDependencies
             , TypeOperators
             , TypeSynonymInstances
               #-}
module Main where
import MFlow.Hack.XHtml.All

import Data.Typeable
import Data.Maybe
import Control.Monad
import System.IO.UTF8


--
--main :: IO ()
--main=   (runFlow $ do
--    let r = return "a" *> return "b" :: View Html IO String
--    FormElm _ x <- lift $ runFormT r
--    liftIO $ print  x)
--    undefined

main= do
   l <- System.IO.UTF8.getLine
   System.IO.UTF8.print l
--
--   userRegister  "pepe" "pepe"
--
--   run 80 $ hackMessageFlow messageFlows
--   where
--   messageFlows=  [("noscript",  error "este es un error")]
--
----
--data a :* b = a :*b deriving Show
--a ||| b = Just (a , b)
--
--
--
--
--main2= do
-- let x= Just "a" ||| Just "b"   ||| Just "c" ||| Just "d" ||| Just "e"
-- print x
-- print (flatten  x :: (Maybe String, Maybe String, Maybe String, Maybe String,Maybe String))
--
--class Next a b c where
--  next :: a -> b -> c
--
--instance  Next a b  (Maybe(Maybe a, Maybe b)) where
--  next a b= Just(Just a, Just b)
--
--instance (Flatten1 a,Next a b d, Iterable a b c) => Flatten1 d c where
--     flatten (Just(ma,mb))= cons ma mb
--     flatten Nothing= cons (flatten Nothing) Nothing
--
--class Flatten1 a b  where
-- flatten1 :: a -> b
--
--
--type First a b= Maybe (Maybe a, Maybe b)
--type Second a b c= Maybe ( (First a b), Maybe c)
--type Third a b c d= Maybe ( (Second a b c), Maybe d)
--type Fourth a b c d e= Maybe ( (Third a b c d), Maybe e)
--type Fifth a b c d e f= Maybe ( (Fourth a b c d e), Maybe f)
--
--instance Flatten1 (First a b) (Maybe a, Maybe b) where
--  flatten1 (Just(ma,mb))= (ma,mb)
--  flatten1 Nothing= (Nothing,Nothing)
--
--instance Flatten1 (Second a b c) (Maybe a, Maybe b,Maybe c) where
--  flatten1 (Just(mx,mc))= let(ma,mb)= flatten1 mx in (ma,mb,mc)
--  flatten1 Nothing= (Nothing,Nothing,Nothing)
--
--instance Flatten1 (Third a b c d) (Maybe a, Maybe b,Maybe c,Maybe d) where
--  flatten1 (Just(mx,mc))= let(ma,mb,md)= flatten1 mx in (ma,mb,md,mc)
--  flatten1 Nothing= (Nothing,Nothing,Nothing,Nothing)
--
--instance Flatten1 (Fourth a b c d e) (Maybe a, Maybe b,Maybe c,Maybe d,Maybe e) where
--  flatten1 (Just(mx,mc))= let(ma,mb,md,me)= flatten1 mx in (ma,mb,md,me,mc)
--  flatten1 Nothing= (Nothing,Nothing,Nothing,Nothing,Nothing)
--
--instance Flatten1 (Fifth a b c d e f) (Maybe a, Maybe b,Maybe c,Maybe d,Maybe e,Maybe f) where
--  flatten1 (Just(mx,mc))= let(ma,mb,md,me,mf)= flatten1 mx in (ma,mb,md,me,mf,mc)
--  flatten1 Nothing= (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)
--
--
--
--
--class Iterable   head tail  cons
--      | cons -> head, cons -> tail, head  tail -> cons  where
--  tcons :: head -> tail -> cons
--  thead :: cons -> head
--  ttail :: cons -> tail
--
--class (Iterable head tail cons
--      , Iterable head1 tail1 cons1
--      , TMap tail tail1 )
--       => TMap  cons cons1 where
--  tmap :: (head-> head1)-> cons -> cons1
--  tmap f x= tcons (f $ thead x) (tmap f $ ttail x)
--
--
--
--instance TMap head tail cons where
--  tmap f x= tcons (thead x) (tmap f $ ttail x)
--
--
--instance Iterable   a  b (a,b) where
--  tcons  x  y= (x,y)
--  thead (x,y)= x
--  ttail (x,y)= y
--
--instance Iterable  a (b,c) (a,b,c) where
--  tcons  x (y,z)= (x,y,z)
--  thead (x,y,z)= x
--  ttail (x,y,z)= (y,z)
--
--
--instance Iterable  a (b,c,d) (a,b,c,d) where
--  tcons x (y,z,t)= (x,y,z,t)
--  thead (x,y,z,t)= x
--  ttail (x,y,z,t)= (y,z,t)
--
