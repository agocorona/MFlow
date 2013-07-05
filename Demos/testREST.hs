import MFlow.Wai.Blaze.Html.All

-- 9 pages , each page has a restful link
main= do
  addMessageFlows[("",transient $ runFlow mainf)]
  wait $ run 80 waiMessageFlow


mainf = do
  liftIO $ print "INIT"
  option <- page $   wlink "a" << "letters "
                 <|> p << "or" ++> wlink "1" << "numbers"

  case option of
    "1" -> do
          page $ wlink "2" << "2"
          page $ wlink "3" << "3"
          page $ wlink "4" << "4"
          page $ wlink  () <<  "menu"

    "a" -> do
          page $ wlink "b" << "b"
          page $ wlink "c" << "c"
          page $ wlink "d" << "d"
          page $ wlink () <<  "menu"


