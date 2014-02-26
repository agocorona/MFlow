
module WebService where

import MFlow.Wai.Blaze.Html.All
import MFlow.Forms.WebApi





-- mainRest= runNavigation "apirest" . step $ ask $
restService :: View Html IO ()
restService= do
     op <- getRestParam
     term1 <- getRestParam
     term2 <- getRestParam
     case (op, term1,term2) of
       (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stop
       (Just "prod", Just x, Just y) ->  wrender (x * y) **> stop
       _  ->do -- blaze Html
                 h1 << "ERROR. API usage:"
                 h3 << "http://server/apirest/sum/[Int]/[Int]"
                 h3 << "http://server/apirest/prod/[Int]/[Int]"
              ++> stop


--mainService= runNavigation "apikv" . step $ ask $
keyValueService :: View Html IO ()
keyValueService= do
     op <- getRestParam
     term1 <- getKeyValueParam "t1"
     term2 <- getKeyValueParam "t2"
     case (op, term1,term2) of
       (Just  "sum", Just x, Just y) ->  wrender (x + y :: Int) **> stop
       (Just "prod", Just x, Just y) ->  wrender (x * y) **> stop
       _  ->do -- blaze Html
                 h1 << "ERROR. API usage:"
                 h3 << "http://server/apikv/sum?t1=[Int]&t2=[Int]"
                 h3 << "http://server/apikv/prod?t1=[Int]&t2=[Int]"
              ++> stop

--mainParser  = runNavigation "apiparser" . step . asks $
-- or
--mainParser =do
--  addMessageFlows[("apiparser",wstateless  parserService)]
--  wait $ run 80 waiMessageFlow

parserService :: View Html IO ()
parserService=
         do rest "sum"  ; disp $ (+) <$> wint "t1" <*> wint "t2"
     <|> do rest "prod" ; disp $ (*) <$> wint "t1" <*> wint "t2"
     <?> do -- blaze Html
            h1 << "ERROR. API usage:"
            h3 << "http://server/apiparser/sum?t1=[Int]&t2=[Int]"
            h3 << "http://server/apiparser/prod?t1=[Int]&t2=[Int]"
    where
    asks w= ask $ w >> stop
    wint p= wparam p :: View Html IO Int



