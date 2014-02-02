{-# OPTIONS -XScopedTypeVariables  -XOverloadedStrings #-}

module MFlow.Cookies (
  CookieT,
  Cookie(..),
  contentHtml,
  cookieuser,
  cookieHeaders,
  getCookies,
  paranoidEncryptCookie,
  paranoidDecryptCookie,
  encryptCookie,
  decryptCookie
  )
where
import Control.Monad(MonadPlus(..), guard, replicateM_, when)
import Data.Char
import Data.Maybe(fromMaybe, fromJust)
import System.IO.Unsafe
import Control.Exception(handle)
import Data.Typeable
import Unsafe.Coerce
import Data.Monoid
import Text.Parsec
import Control.Monad.Identity
import Data.ByteString.Char8 as B
import Web.ClientSession
import System.Environment

--import Debug.Trace
--(!>)= flip trace

contentHtml :: (ByteString, ByteString)
contentHtml= ("Content-Type", "text/html")

type CookieT =  (B.ByteString,B.ByteString,B.ByteString,Maybe B.ByteString)

data Cookie
  = UnEncryptedCookie CookieT
  | EncryptedCookie CookieT
  | ParanoidCookie CookieT
    deriving (Eq, Read, Show)

cookieuser :: String
cookieuser= "cookieuser"

getCookies httpreq=
     case  lookup "Cookie" $ httpreq of
             Just str  -> splitCookies str :: [(B.ByteString, B.ByteString)]
             Nothing   -> []

cookieHeaders cs =  Prelude.map (\c-> ( "Set-Cookie", showCookie c)) cs

showCookie :: Cookie -> B.ByteString
showCookie c@(EncryptedCookie b) = showCookie' $ decryptAndToTuple c
showCookie c@(ParanoidCookie b)  = showCookie' $ decryptAndToTuple c
showCookie   (UnEncryptedCookie c) = showCookie' c

showCookie' (n,v,p,me) = n <> "="  <>  v  <>
                         ";path="  <>  p  <>
                         showMaxAge  me

showMaxAge Nothing =  ""
showMaxAge (Just e)  =  ";Max-age=" <> e

splitCookies cookies  = f cookies []  
    where
    f s r | B.null s  = r
    f xs0 r =
      let
          xs   = B.dropWhile (==' ') xs0
          name = B.takeWhile (/='=') xs
          xs1  = B.dropWhile (/='=') xs
          xs2  = B.dropWhile (=='=') xs1
          val  = B.takeWhile (/=';') xs2
          xs3  = B.dropWhile (/=';') xs2
          xs4  = B.dropWhile (==';') xs3
          xs5  = B.dropWhile (==' ') xs4
      in  f xs5 ((name,val):r)

-----------------------------

---------------------------------------------

-- %***************************************************************************
-- %*                                                                         *
-- \subsection[CGI-Parser]{Yet another combinator parser library}. chuck of code taken from Erik Meijer
-- %*                                                                         *
-- %***************************************************************************

-- NOTE: This is all a little bit of a sledgehammer here for the simple task
-- at hand...

-- The parser monad

--
--newtype Parser a = Parser (String -> [(a,String)])
--
--instance Functor Parser where
--   -- map :: (a -> b) -> (Parser a -> Parser b)
--   fmap f (Parser p) = Parser (\inp -> [(f v, out) | (v, out) <- p inp])
--
--instance Monad Parser where
--   -- return :: a -> Parser a
--   return v = Parser (\inp -> [(v,inp)])
--
--   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
--   (Parser p) >>= f = Parser (\inp -> concat [papply (f v) out
--                                             | (v,out) <- p inp])
--
--instance MonadPlus Parser where
--   -- zero :: Parser a
--   mzero = Parser (\_ -> [])
--   -- (++) :: Parser a -> Parser a -> Parser a
--   (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))
--       
--
---- Other primitive parser combinators
--
--       
--item :: Parser Char
--item = Parser (\inp -> case inp of
--                     []     -> []
--                     (x:xs) -> [(x,xs)])
--
--force :: Parser a -> Parser a
--force (Parser p) = Parser (\inp -> let x = p inp in
--                             (fst (head x), snd (head x)) :  tail x)
--
--first :: Parser a -> Parser a
--first (Parser p) = Parser (\inp -> case p inp of
--                            []    -> []
--                            (x:_) -> [x])
--
--papply :: Parser a -> String -> [(a,String)]
--papply (Parser p) inp = p inp
--       
--
---- Derived combinators
--
--       
--(+++) :: Parser a -> Parser a -> Parser a
--p +++ q = first (p `mplus` q)
--
--sat :: (Char -> Bool) -> Parser Char
--sat p = do {x <- item; guard (p x); return x}
--
--many :: Parser a -> Parser [a]
--many p = force (many1 p +++ return [])
--
--many1 :: Parser a -> Parser [a]
--many1 p = do {x <- p; xs <- many p; return (x:xs)}
--
--sepby :: Parser a -> Parser b -> Parser [a]
--p `sepby` sep = (p `sepby1` sep) +++ return []
--
--sepby1 :: Parser a -> Parser b -> Parser [a]
--p `sepby1` sep = do x  <- p
--                    xs <- many (do {sep; p})
--                    return(x:xs)
--
--char :: Char -> Parser Char
--char x = sat (x==)
--
--alphanum :: Parser Char
--alphanum = sat (\c -> isAlphaNum c || c == '@' || c =='\'' )    -- Added @ as a valid character
--
--string :: String -> Parser String
--string ""     = return ""
--string (x:xs) = do char x
--                   string xs
--                   return (x:xs)
--
--hexdigit :: Parser Char
--hexdigit = sat isHexDigit
--       
--
--readEnv :: Parser [(String,String)] 
readEnv = (do
          n <-  urlEncoded
          string "="
          v <-  urlEncoded
          return (n,v)) `sepBy` (string "&")

urlEncoded :: Parsec String ()  String
urlEncoded
 = many ( alphaNum `mplus` extra `mplus` safe
         `mplus` do{ char '+' ; return ' '}
         `mplus` do{ char '%' ; hexadecimal }                 
         )


--extra :: Parser Char
extra = satisfy (`Prelude.elem` "!*'(),/\"")
--
--safe :: Parser Char
safe = satisfy (`Prelude.elem` "$-_.") 
----
--hexadecimal :: Parser HexString
hexadecimal = do d1 <- hexDigit
                 d2 <- hexDigit
                 return .chr $ toInt d1* 16 + toInt d2
   where toInt d | isDigit d    =  ord d - ord '0'
         toInt d | isHexDigit d = (ord d - ord 'A') + 10
         toInt d                = error ("hex2int: illegal hex digit " ++ [d])
--type HexString = String

--hex2int :: HexString -> Int
--hex2int ds = Prelude.foldl (\n d -> n*16+d) 0 (Prelude.map (toInt . toUpper) ds)
--   where toInt d | isDigit d    =  ord d - ord '0'
--         toInt d | isHexDigit d = (ord d - ord 'A') + 10
--         toInt d                = error ("hex2int: illegal hex digit " ++ [d])

--urlDecode :: String -> [([(String, String)],String)]
--urlDecode str= case parse readEnv "" str of  -- let Parser p= readEnv in  p str
--                     Left err  -> error $ "urlDecode: decode  error: " ++ show err



--                     Right r  ->   r
--               !> ("decode="++str)

decryptCookie :: Cookie -> IO Cookie
decryptCookie c@(UnEncryptedCookie _) = return c
decryptCookie   (EncryptedCookie c)   = decryptCookie' c
decryptCookie   (ParanoidCookie c)    = paranoidDecryptCookie c

-- Uses 4 seperate keys, corresponding to the 4 seperate fields in the Cookie.
paranoidEncryptCookie :: CookieT -> IO Cookie
paranoidEncryptCookie (a,b,c,d) = do
  key1 <- getKey "CookieKey1.key"
  key2 <- getKey "CookieKey2.key"
  key3 <- getKey "CookieKey3.key"
  key4 <- getKey "CookieKey4.key"
  iv1  <- randomIV
  iv2  <- randomIV
  iv3  <- randomIV
  iv4  <- randomIV
  return $ ParanoidCookie
             ( encrypt      key1 iv1 a,
               encrypt      key2 iv2 b,
               encrypt      key3 iv3 c,
               encryptMaybe key4 iv4 d)

paranoidDecryptCookie :: CookieT -> IO Cookie
paranoidDecryptCookie (a,b,c,d) = do
  key1 <- getKey "CookieKey1.key"
  key2 <- getKey "CookieKey2.key"
  key3 <- getKey "CookieKey3.key"
  key4 <- getKey "CookieKey4.key"
  return $ UnEncryptedCookie
             ( decryptFM    key1 a,
               decryptFM    key2 b,
               decryptFM    key3 c,
               decryptMaybe key4 d)

-- Uses a single key to encrypt all 4 fields.
encryptCookie :: CookieT -> IO Cookie
encryptCookie (a,b,c,d) = do
  key <- getKey  "CookieKey.key"
  iv1  <- randomIV
  iv2  <- randomIV
  iv3  <- randomIV
  iv4  <- randomIV
  return $ EncryptedCookie
             ( encrypt      key iv1 a,
               encrypt      key iv2 b,
               encrypt      key iv3 c,
               encryptMaybe key iv4 d)

decryptCookie' :: CookieT -> IO Cookie
decryptCookie' (a,b,c,d) = do
  key <- getKey "CookieKey.key"
  return $ UnEncryptedCookie
             ( decryptFM    key a,
               decryptFM    key b,
               decryptFM    key c,
               decryptMaybe key d)

encryptMaybe :: Key -> IV -> Maybe ByteString -> Maybe ByteString
encryptMaybe k i (Just s) = Just $ encrypt k i s
encryptMaybe _ _ Nothing  = Nothing

decryptMaybe :: Key -> Maybe ByteString -> Maybe ByteString
decryptMaybe k (Just s) = Just $ fromMaybe "" $ decrypt k s
decryptMaybe _ Nothing  = Nothing

decryptFM :: Key -> ByteString -> ByteString
decryptFM k b = fromMaybe "" $ decrypt k b

cookieToTuple :: Cookie -> CookieT
cookieToTuple (UnEncryptedCookie c) = c
cookieToTuple (EncryptedCookie c) = c
cookieToTuple (ParanoidCookie c) = c

decryptAndToTuple :: Cookie -> CookieT
decryptAndToTuple = cookieToTuple . unsafePerformIO . decryptCookie
