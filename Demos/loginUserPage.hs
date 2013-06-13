{-# LANGUAGE  CPP, DeriveDataTypeable, ScopedTypeVariables #-}

-- #define TEST
module Main where
import MFlow.Wai.Blaze.Html.All as MF
#ifdef TEST
  hiding (ask,userWidget)
import MFlow.Forms.Test
#endif
import Data.Typeable
import Data.String(fromString)
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery as Q
import Data.TCache.IndexText
import Data.Maybe
import qualified Data.Map as M
import Control.Workflow.Configuration
import Control.Workflow (Workflow)
import Data.Text.Lazy as T

import Debug.Trace
(!>) = flip trace

main= do
   Q.index namep
   indexList typep (Prelude.map T.pack)
   indexText descriptionp T.pack
   runConfiguration "createprods" $ once createProducts
#ifdef TEST
   runTest1 $ runFlowOnce pages
   runTest1 $ runFlowOnce skins
#else
   addMessageFlows  [(""    ,  transient $ runFlow pages)
                    ,("skins", runFlow skins)
                    ,("shop",  runFlow shop )
                    ,("hello", runFlow $ helloWorld 0)
                    ,("sum", transient $ runFlow sumit)]
   wait $ run 80 waiMessageFlow
#endif

helloWorld n= do
   step $ ask $ h1 << ("this is the Hello World number:" ++ show n) ++> wlink () << b << "press here"
   helloWorld $ n + 1

sumit= do
       setHeader $ html . body
       n1 <- ask $  p << "give me the first number"  ++>  getInt Nothing
       n2 <- ask $  p << "give me the second number" ++>  getInt Nothing
       ask $ p << ("the result is " ++ show (n1 + n2)) ++> wlink () << p << "click here"
       sumit


pages=  do
     r<- ask landingPage
     case r of
       "myPage" -> do
         user <- ask loginPage
         ask $ userPage user
       "otherPage" -> error "not implemented"

landingPage= p << "hi, this is the landing page"
               ++> wlink "myPage" << p << "press here to go to your page"
               <|> wlink "otherPage" << p << "or press here if you like to go to other page"

loginPage= b << "please login" ++> userWidget Nothing userLogin

userPage user = p << ("this is your user page for " ++ user)
                  ++> wlink () << p << "press to go to landingPage"


data Skin= Normal |  Blue | Red deriving (Read,Show,Typeable, Bounded, Enum)

skins= do
   setHeader $  html . body
   setTimeouts 120 (365*24*60*60)
   skin <- step . ask $   p << "choose skin"
                     ++> wlink Normal << p << "normal"
                     <|> wlink Blue    << p << "blue"
                     <|> wlink Red     << p << "red"
   step $ restOfTheFlow skin

   where
   restOfTheFlow skin = do
        r  <- ask $   p << ("you choosen the skin " ++ show skin)
                  ++> wlink "change" << MF.div << "Change the skin"
                  <++ br
                  <|> wlink "doOther" << p << "other things"
        case r of
           "change" -> breturn ()
           _ ->do
                  ask $   p << "other things"
                      ++> a ! href (fromString "http://www.google.com") << p << "google"
                      ++> br
                      ++> wlink() << p << "press here to return to the salutation"
                  restOfTheFlow  skin

type ProductName= String
type Quantity= Int
type Price= Float

type Cart= M.Map ProductName (Quantity, Price)

showCart :: Cart -> String
showCart = show


data Product= Product{ namep :: String
                     , typep :: [String]
                     , descriptionp :: String
                     , pricep :: Price
                     , stock :: Int}
              deriving (Read,Show,Typeable)

instance Indexable Product where
   key prod= "Prod "++ namep prod

createProducts= atomically $ mapM newDBRef
    [ Product "ipad 3G"   ["gadget","pad"]   "ipad 8GB RAM, 3G"       400 200
    , Product "ipad"      ["gadget","pad"]   "ipad 8 GB RAM"           300 300
    , Product "iphone 3"  ["gadget","phone"] "iphone 3 nice and beatiful"  200 100
    ]

shop :: FlowM Html (Workflow IO) ()
shop = do
   setHeader $ html . body
   setTimeouts 120 (30*24*60*60)
   catalog
   where
   catalog = do
       bought <-  step $ buyProduct
       shoppingCart bought
       catalog

   shoppingCart bought= do
       cart <- getSessionData `onNothing` return (M.empty ::Cart)
       let (n,price) = fromMaybe (0,undefined) $ M.lookup  bought cart
       (n,price) <-step $ do
                   if n /= 0 then return (n,price) else do
                    [price] <- atomic $ Q.select pricep $ namep .==. bought
                    return (n, price)
       setSessionData $ M.insert  bought (n+1,price) cart
       step $ do
         r <- ask $  do
              cart <- getSessionData `onNothing` return (M.empty :: Cart)
              h1 << "Shopping cart:"
                ++> p << showCart cart
                ++> wlink True  << b << "continue shopping"
                <|> wlink False << p << "proceed to buy"

         if not r then ask $ wlink () << "not implemented, click here" !> show r
              else breturn ()

   atomic= liftIO . atomically
   showList []= wlink Nothing << p << "no results"
   showList xs= Just <$> firstOf [wlink  x << p <<  x | x <- xs]

   buyProduct :: FlowM Html  IO ProductName
   buyProduct =  do
        ttypes   <-  atomic $ allElemsOf typep
        let types= Prelude.map T.unpack ttypes
        r  <- ask $   h1 << "Product catalog"
                  ++> p << "search" ++> (Left <$> getString Nothing)
                  <|> p << "or choose product types" ++>  (Right <$> showList types)

        prods <- case r of
          Left str           -> atomic $ Q.select namep $ descriptionp `contains` str
          Right (Just type1) -> atomic $ Q.select namep $ typep `containsElem` type1
          Right Nothing      -> return []

        if Prelude.null prods then buyProduct else do
            let search= case r of
                            Left str ->              "for search of the term " ++ str
                            Right (Just type1) ->    "of the type "++ type1

            r <- ask $ h1 << ("Products " ++ search) ++> showList prods
            case r of
              Nothing   -> buyProduct
              Just prod -> breturn prod



