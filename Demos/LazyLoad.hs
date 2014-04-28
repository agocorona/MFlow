{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings #-}
module LazyLoad (lazyLoad) where
import Data.Typeable

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "showResults" . transientNav $ do
    setHeader $ docTypeHtml . body
    lazyLoad
#else
import MFlow.Wai.Blaze.Html.All hiding(page)
import Menu
#endif

data Opts=  Sequence | Recursive deriving(Show, Typeable)

lazyLoad=  do
    r <- page  $ wlink Sequence << p "Lazy present the 20 numbers"
             <|> wlink Recursive << p "present 20 numbers lazily recursive"
    r <- case r of
       Sequence  -> page $ pageFlow "lazy" $ lazyPresent  (0 :: Int) 20
       Recursive -> page $ pageFlow "lazy" $ lazyPresentR (0 :: Int) 20
    page $ wlink () << p << (show r ++ " selected. Go to home" )

lazyPresent i n=  firstOf[lazy  spinner (wlink i $ p << (show i) ) | i <- [i..n :: Int]]

lazyPresentR i n
   | i == n= noWidget
   | otherwise= wlink i << p << (show i) <|> lazy spinner (lazyPresentR (i+1) n)

spinner= img ! src "//ganglia.wikimedia.org/latest/img/spinner.gif"

