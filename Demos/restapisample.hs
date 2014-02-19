
module Main where

import MFlow.Wai.Blaze.Html.All

import Control.Monad.State
import Debug.Trace

(!>)= flip trace

stop= noWidget

main2= runNavigation "api" . step $ ask $ do
         op <- getRestParam
         term1 <- getRestParam
         term2 <- getRestParam
         case (op, term1,term2) of
           (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stop
           (Just "prod", Just x, Just y) ->  wrender (x * y) **> stop
           _  ->do -- blaze Html
                     h1 << "ERROR. API usage:"
                     h3 << "http://server/api/sum/[Int]/[Int]"
                     h3 << "http://server/api/prod/[Int]/[Int]"
                  ++> stop


main1= runNavigation "api" . step $ ask $ do
         op <- getRestParam
         term1 <- getKeyValueParam "t1"
         term2 <- getKeyValueParam "t2"
         case (op, term1,term2) of
           (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stop
           (Just "prod", Just x, Just y) ->  wrender (x * y) **> stop
           _  ->do -- blaze Html
                     h1 << "ERROR. API usage:"
                     h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                     h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
                  ++> stop

wrestParam = View $ do
   mr <- getRestParam
   return $ FormElm [] mr

rest v= do
   r <- wrestParam
   if r==v then return v else modify (\s -> s{mfPIndex= mfPIndex s-1}) >> stop

wparam par= View $ do
   mr <- getKeyValueParam par
   return $ FormElm [] mr


--disp w= View $ do
--   elm@(FormElm f mx) <- runView w
--   case mx of
--     Nothing -> return elm
--     justx@(Just x) -> return $ FormElm (f++[fromStr $ show x]) justx

main  = runNavigation "api" . step . ask $
             do rest "sum"  ; wrender ((+) <$> wint "t1" <*> wint "t2") **> stop
         <|> do rest "prod" ; wrender ((*) <$> wint "t1" <*> wint "t2") **> stop
         <|> do -- blaze Html
                 h1 << "ERROR. API usage:"
                 h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                 h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
              ++> stop



wint p= wparam p :: View Html IO Int



