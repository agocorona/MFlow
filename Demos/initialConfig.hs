{-# LANGUAGE  CPP, DeriveDataTypeable, ScopedTypeVariables #-}

-- #define TEST
module InitialConfig (initialConfig) where
-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All as MF
main= runNavigation "" initialConfig
#else
import MFlow.Wai.Blaze.Html.All as MF hiding(retry, page)
import Menu
#endif
import Data.Typeable
import Data.String(fromString)


data Skin= Normal |  Blue | Red deriving (Read,Show,Typeable, Bounded, Enum)

initialConfig= do
   mskin <- getSessionData
   skin <- case mskin of
    Nothing -> do
     s <- step . page $ p << "choose skin"
                    ++> wlink Normal << p << "normal"
                    <|> wlink Blue    << p << "blue"
                    <|> wlink Red     << p << "red"
     setSessionData s
     return s
    Just sk -> return sk
   step $ restOfTheFlow skin

   where
   restOfTheFlow skin = do
        r  <- page  $ p << ("skin chosen so far: "++ show skin)
                  ++> p << ("you choosen the skin " ++ show skin)
                  ++> wlink "change" << MF.div << "Change the skin"
                  <++ br
                  <|> wlink "doOther" << p << "other things"
        case r of
           "change" -> breturn ()
           _ ->do
                  page  $ p << ("skin chosen so far: "++ show skin)
                      ++> p << "other things"
                      ++> a ! href (fromString "http://www.google.com") << p << "google"
                      ++> br
                      ++> wlink() << p << "press here to return to the salutation"
                  restOfTheFlow  skin


