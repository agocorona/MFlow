{-# OPTIONS -XExistentialQuantification  -XTypeSynonymInstances
            -XFlexibleInstances -XDeriveDataTypeable -XOverloadedStrings #-}
module MFlow.Wai.Response where

import Network.Wai
import MFlow.Cookies
import Data.ByteString.Char8 as SB
import Data.ByteString.Lazy.Char8 as B
import MFlow
import Data.Typeable
import Data.Monoid
import System.IO.Unsafe
import Data.Map as M
--import Data.CaseInsensitive
import Network.HTTP.Types
import Control.Workflow(WFErrors(..))
import Data.String
--import Debug.Trace
--
--(!>)= flip trace



class ToResponse a where
      toResponse :: a -> Response



data TResp = TRempty | forall a.ToResponse a=>TRespR a |  forall a.(Typeable a, ToResponse a, Monoid a) => TResp a deriving Typeable

instance Monoid TResp where
      mempty = TRempty
      mappend (TResp x) (TResp y)=
         case cast y of
              Just y' -> TResp $ mappend x y'
              Nothing -> error $ "fragment of type " ++ show ( typeOf  y)  ++ " after fragment of type " ++ show ( typeOf x)


contentHtml1= [mkparam contentHtml] -- [(mk $ SB.pack "Content-Type", SB.pack  "text/html")]
mkParams = Prelude.map mkparam
mkparam (x,y)= (fromString  x, fromString y)
instance ToResponse TResp where
  toResponse (TResp x)= toResponse x
  toResponse (TRespR r)= toResponse r
  
instance ToResponse Response where
      toResponse = id

instance ToResponse B.ByteString  where
      toResponse x= responseLBS status200 contentHtml1 {-,("Content-Length",show $ B.length x) -} x

instance ToResponse String  where
      toResponse x= responseLBS status200 contentHtml1 {-,("Content-Length",show $ B.length x) -} $ B.pack x

instance  ToResponse HttpData  where
  toResponse (HttpData hs cookies x)= responseLBS status200 (mkParams ( hs ++ cookieHeaders cookies)) x
  toResponse (Error NotFound str)=  error "FATAL ERROR: HttpData errors should not reach here: MFlow.Forms.Response.hs " -- responseLBS status404 [] $ (unsafePerformIO  getNotFoundResponse) str


