-----------------------------------------------------------------------------
--
-- Module      :  OrElse
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

module OrElse (

) where

import MFlow.Wai.Blaze.Html.All
import System.Time
import Control.Concurrent

import Debug.Trace
(!>)= flip trace


main= runNavigation "orelse" $ step $ test


test= do
    time <- liftIO getClockTime

    r<-(liftIO $ threadDelay 5000000 >> return "ho")
      `orElse`
         ask ((p << show time ++> wlink "hi" << "hi") !> "render")
    liftIO $ print r
    return ()


