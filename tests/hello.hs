import MFlow.Wai.Blaze.Html.All

main= runNavigation "hello" . step $ do
      name <- page $  b << "your name?" ++> getString Nothing <** submitButton "ok"
      page $ b << ("hello " ++ name) ++> wlink ()  << b << "again?"
