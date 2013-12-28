-----------------------------------------------------------------------------
--
-- Module      :  Todo
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

module Todo (

) where

import MFlow.Forms.Wai.Blaze.Html.All


main= runNavigation "todo" $ transientNav $ page $ witerate do
        new <- getString Nothing <! [("","Enter new task here")] >>= return . Just
             `onNothing` return Nothing
        todos  <- readDBRef rtodos `onNothing` return  []
        todos' <- case new of
          Nothing -> return todos
          Just todo -> do
              let todos'= todo:todos
              writeDBRef rtodos  (todos'
              return todos'
        dfield . wraw $ ul [li $ todo | todo <- todos']
