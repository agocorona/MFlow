
module Main where

import MFlow.Wai.Blaze.Html.All




main= runNavigation "api" . step $ ask $ do
         op <- getRestParam
         term1 <- getRestParam
         term2 <- getRestParam
         case (op, term1,term2) of
           (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> noWidget
           (Just "prod", Just x, Just y) ->  wrender (x * y)  **> noWidget
           _  ->do -- blaze Html
                     h1 << "ERROR. API usage:"
                     h3 << "http://server/api/sum/[Int]/[Int]"
                     h3 << "http://server/api/prod/[Int]/[Int]"
                ++>noWidget
