{-# OPTIONS -XExistentialQuantification  -XTypeSynonymInstances
            -XFlexibleInstances -XDeriveDataTypeable #-}
module MFlow.Hack.Response where

import Hack
import MFlow.Cookies
import Data.ByteString.Lazy.Char8
import MFlow(HttpData(..))
import Data.Typeable
import Data.Monoid

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



defaultResponse :: String ->  IO Response
defaultResponse  msg=  return  . toResponse $ "<p>Page not found or error ocurred:<br/>" ++ msg ++  "<br/><a href=\"/\" >home</a> </p>"

errorResponse msg=
   "<h4>Page not found or error ocurred:<h4/></h4><h3>" ++ msg ++
   "</h3><br/> <a href=\"/\" >press here to go home</a> </p>"


instance ToResponse TResp where
  toResponse (TResp x)= toResponse x
  toResponse (TRespR r)= toResponse r
  
instance ToResponse Response where
      toResponse = id

instance ToResponse ByteString  where
      toResponse x= Response{status=200, headers=[ctype {-,("Content-Length",show $ B.length x) -}], body= x}

instance ToResponse String  where
      toResponse x= Response{status=200, headers=[ctype{-,("Content-Length",show $ B.length x) -}], body= pack x}




instance  ToResponse HttpData  where
  toResponse (HttpData cookies x)=   (toResponse x) {headers= cookieHeaders cookies}



instance Typeable Env where
     typeOf = \_-> mkTyConApp (mkTyCon "Hack.Env") []

instance Typeable Response where
     typeOf = \_-> mkTyConApp (mkTyCon "Hack.Response")[]
