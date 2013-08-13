module DemoMenu where

import MFlow.Wai.Blaze.Html.All




main= runNavigation "" . transientNav $ do
  option <- ask $ menu1 <++ p << "choose"
  case option of
    "1" -> do
           pagem $ wlink "2" << contentFor "1"
           pagem $ wlink "3" << contentFor "2"
           pagem $ wlink "4" << contentFor "3"

    "a" -> do
           pagem $ wlink "b" << contentFor "a"
           pagem $ wlink "c" << contentFor "b"
           pagem $ wlink "d" << contentFor "c"

  pagem $ wlink ()  << p << "back to the first page"

menu1 =  wcached "menu" 0 $ wlink "a" << b << "letters " <++ i << "or "   <|> wlink "1" << b << "numbers"



pagem  pagecode =page $ retry menu1 **> pagecode



contentFor x= do
        p << "page for"
        b << x
        p << "goto next page"

header1= html . body


