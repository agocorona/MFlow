
module Main where

import MFlow.Wai.Blaze.Html.All

import Control.Monad.State
import MFlow.Forms.Internals
import Debug.Trace

(!>)= flip trace

stayInPage= noWidget

main2= runNavigation "api" . step $ ask $ do
         op <- getRestParam
         term1 <- getRestParam
         term2 <- getRestParam
         case (op, term1,term2) of
           (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stayInPage
           (Just "prod", Just x, Just y) ->  wrender (x * y) **> stayInPage
           _  ->do -- blaze Html
                     h1 << "ERROR. API usage:"
                     h3 << "http://server/api/sum/[Int]/[Int]"
                     h3 << "http://server/api/prod/[Int]/[Int]"
                  ++> stayInPage


main1= runNavigation "api" . step $ ask $ do
         op <- getRestParam
         term1 <- getParam "t1"
         term2 <- getParam "t2"
         case (op, term1,term2) of
           (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stayInPage
           (Just "prod", Just x, Just y) ->  wrender (x * y) **> stayInPage
           _  ->do -- blaze Html
                     h1 << "ERROR. API usage:"
                     h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                     h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
                  ++> stayInPage

wrestParam = View $ do
   mr <- getRestParam
   return $ FormElm [] mr

wverb v= do
   r <- wrestParam
   if r==v then return v else modify (\s -> s{mfPIndex= mfPIndex s-1}) >> noWidget

wparam par= View $ do
   mr <- getParam par
   return $ FormElm [] mr


disp w= View $ do
   elm@(FormElm f mx) <- runView w
   case mx of
     Nothing -> return elm
     justx@(Just x) -> return $ FormElm (f++[fromStr $ show x]) justx

main  = runNavigation "api" . step . ask $
             do wverb "sum"  ; disp ((+) <$> wint "t1" <*> wint "t2") **> stayInPage
         <|> do wverb "prod" ; disp ((*) <$> wint "t1" <*> wint "t2") **> stayInPage
         <|> do -- blaze Html
                 h1 << "ERROR. API usage:"
                 h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                 h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
              ++> stayInPage



wint p= wparam p :: View Html IO Int



