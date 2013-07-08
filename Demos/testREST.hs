import MFlow.Wai.Blaze.Html.All

-- 9 pages , each page has a restful link (page = ask)
main= runNavigation $ do
  liftIO $ print "start/restart"

  setHeader $ html . body

  option <- page $   wlink "a" << p << "letters " <++ p << "or"
                 <|> wlink "1" << p << "numbers"

  case option of
    "1" -> do
          page $ wlink "2" << p << "2"
          page $ wlink "3" << p << "3"
          page $ wlink "4" << p << "4"
          page $ wlink ()  << p << "menu"

    "a" -> do
          page $ wlink "b" << p << "b"
          page $ wlink "c" << p << "c"
          page $ wlink "d" << p << "d"
          page $ wlink ()  << p << "menu"


