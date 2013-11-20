{-# OPTIONS   -XOverloadedStrings #-}
import MFlow.Wai.Blaze.Html.All
import Text.Blaze.Html5 as El
import Text.Blaze.Html5.Attributes as At hiding (step)
import Data.String

main= runNavigation "" $ step $ do

  ask $ wlink () "press hers"
  r <- ask $ getString Nothing
  ask $ wraw $ b << r

wikip= "wiki/"


