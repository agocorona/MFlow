-----------------------------------------------------------------------------
--
-- Module      :  ApplicativeRoutes
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

module ApplicativeRoutes (
appRoutes
) where

import MFlow.Wai.Blaze.Html.All

main  = runNavigation "approutes" . step . asks $
             do rest "hello"  ; hello
         <|> do rest "hi" ; hi
         <|> do welcome
         <?> do -- blaze Html
                h1 << "ERROR. API usage:"
                h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
                h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
    where
    asks w= ask $ w >> stop

welcome= wraw $ do
     h1 $ hi

hello= B
