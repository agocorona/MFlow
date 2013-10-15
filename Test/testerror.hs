module Main where
import MFlow.Wai.Blaze.Html.All

main= runNavigation "" . transientNav $ do
   page $ wlink () << "press"
   error "error"

