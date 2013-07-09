import MFlow.Wai.Blaze.Html.All

-- 9 pages , each page has a restful link (page = ask)
main= runNavigation "" $ transientNav $ do
  liftIO $ print "start/restart"

  setHeader $ html . body

  option <- page $   wlink "a" << p << "letters " <++ p << "or"
                 <|> wlink "1" << p << "numbers"

  case option of
    "1" -> do
          page $ wlink "2" << cont "2"
          page $ wlink "3" << cont "3"
          page $ wlink "4" << cont "4"
          page $ wlink ()  << cont "menu"

    "a" -> do
          page $ wlink "b" << cont "b"
          page $ wlink "c" << cont "c"
          page $ wlink "d" << cont "d"
          page $ wlink ()  << cont "menu"


cont x= p << ("page for " ++ x)
