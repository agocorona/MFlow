{-# OPTIONS   -XCPP -XDeriveDataTypeable #-}
module SearchCart (
 searchCart
) where
import Data.TCache.DefaultPersistence
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery as Q
import Data.TCache.IndexText
import Data.Maybe
import qualified Data.Map as M
import Control.Workflow.Configuration
import Control.Workflow (Workflow)
import qualified Data.Text.Lazy as T
import Data.ByteString.Lazy.Char8
import Data.Typeable

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" searchCart
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

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

instance Serializable Product where
  serialize= pack . show
  deserialize= read . unpack
  setPersist= const $ Just filePersist

createProducts= atomically $ mapM newDBRef
    [ Product "ipad 3G"   ["gadget","pad"]   "ipad 8GB RAM, 3G"       400 200
    , Product "ipad"      ["gadget","pad"]   "ipad 8 GB RAM"           300 300
    , Product "iphone 3"  ["gadget","phone"] "iphone 3 nice and beatiful"  200 100
    ]

searchCart :: FlowM Html (Workflow IO) ()
searchCart = do
   liftIO $ runConfiguration "createprods" $ do  -- better put this in main
        ever $ Q.index namep
        ever $ indexList typep (Prelude.map T.pack)
        ever $ indexText descriptionp T.pack
        once createProducts
   catalog
   where
   catalog = do
       bought <-  step  buyProduct
       shoppingCart bought
       catalog



   buyProduct :: FlowM Html  IO ProductName
   buyProduct =  do
        ttypes   <-  atomic $ allElemsOf typep
        let types= Prelude.map T.unpack ttypes
        r  <- page $   h1 << "Product catalog"
                  ++> p << "search" ++> (Left <$> getString Nothing)
                  <|> p << "or choose product types" ++>  (Right <$> showList types)

        prods <- case r of
          Left str           -> atomic $ Q.select namep $ descriptionp `contains` str
          Right (Just type1) -> atomic $ Q.select namep $ typep `containsElem` type1
          Right Nothing      -> return []

        if Prelude.null prods
         then do
             page $ b << "no match " ++> wlink () << b << "search again"
             buyProduct
         else do
            let search= case r of
                            Left str ->              "for search of the term " ++ str
                            Right (Just type1) ->    "of the type "++ type1

            r <- page $ h1 << ("Products " ++ search) ++> showList prods
            case r of
              Nothing   -> buyProduct
              Just prod -> breturn prod

   shoppingCart bought= do
       cart <- getSessionData `onNothing` return (M.empty :: Cart)
       let (n,price) = fromMaybe (0,undefined) $ M.lookup  bought cart
       (n,price) <- step $ do
                   if n /= 0 then return (n,price) else do
                    [price] <- atomic $ Q.select pricep $ namep .==. bought
                    return (n, price)
       setSessionData $ M.insert  bought (n+1,price) cart
       step $ do
         r <- page $  do
              cart <- getSessionData `onNothing` return (M.empty :: Cart)
              h1 << "Shopping cart:"
                ++> p << showCart cart
                ++> wlink True  << b << "continue shopping"
                <|> wlink False << p << "proceed to buy"

         if not r then page $ wlink () << "not implemented, click here"
              else breturn ()


   atomic= liftIO . atomically
   showList []= wlink Nothing << p << "no results"
   showList xs= Just <$> firstOf [wlink  x << p <<  x | x <- xs]
