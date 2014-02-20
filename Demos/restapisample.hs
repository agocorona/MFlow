
module Main where

import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.Internals
import Control.Monad.State
import Debug.Trace





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

main  = runNavigation "api" . step . asks $
             do rest "sum"  ; disp $ (+) <$> wint "t1" <*> wint "t2"
         <|> do rest "prod" ; disp $ (*) <$> wint "t1" <*> wint "t2"
         <?> do -- blaze Html
                h1 << "ERROR. API usage:"
                h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
    where
    asks w= ask $ w >> stop

-- To add to WService.hs  or so --

stop= noWidget

wrestParam = View $ do
   mr <- getRestParam
   return $ FormElm [] mr

rest v= do
   r <- wrestParam
   if r==v then return v else modify (\s -> s{mfPIndex= mfPIndex s-1}) >> stop

wparam par= View $ do
   mr <- getKeyValueParam par
   return $ FormElm [] mr

disp :: Show a => View Html IO a -> View Html IO ()
disp w= View $ do
   elm@(FormElm f mx) <- runView w
   case mx of
     Nothing -> return $ FormElm f Nothing
     justx@(Just x) -> return $ FormElm (f++[fromStr $ show x]) $ return ()





infixl 3 <?>
(<?>) w v= View $ do
  r@(FormElm f mx) <- runView w
  case mx of
    Nothing -> runView $ v ++> stop
    Just _ -> return r

wint p= wparam p :: View Html IO Int



