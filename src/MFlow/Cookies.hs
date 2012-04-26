module MFlow.Cookies
(Cookie,ctype,getCookies,cookieHeaders,urlDecode,cookieuser)
where
import Control.Monad(MonadPlus(..), guard, replicateM_, when)
import Data.Char
import Data.Maybe(fromMaybe)
import System.IO.Unsafe
import Control.Exception(handle)
import Data.Typeable
import Data.Maybe(fromJust)
import Unsafe.Coerce

type Cookie=  (String,String,String,Maybe String)

cookieuser= "cookieuser"
getCookies httpreq=
     case  lookup "Cookie" $ httpreq of
             Just str  -> {-# SCC "splitCookies" #-}splitCookies str
             Nothing   -> []

cookieHeaders cs =  map (\c-> ("Set-Cookie", showCookie c)) cs

showCookie ::  Cookie -> String
showCookie (n,v,p,me) =
   let e= fromMaybe "" me
   in showString n . showString "=" . showString v  . showString "; path=" 
     . showString p . showExpires e  . showString "\n"  $ ""


showExpires [] = showString ""
showExpires e  = showString "; expires=" . showString e




ctype= ("Content-Type", "text/html")



splitCookies cookies = f cookies []  
    where
    f [] r = r
    f xs0 r =
      let
          xs   = dropWhile (==' ') xs0
          name = takeWhile (/='=') xs
          xs1  = dropWhile (/='=') xs
          xs2  = dropWhile (=='=') xs1
          val  = takeWhile (/=';') xs2
          xs3  = dropWhile (/=';') xs2
          xs4  = dropWhile (==';') xs3
          xs5  = dropWhile (==' ') xs4
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


newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
   -- map :: (a -> b) -> (Parser a -> Parser b)
   fmap f (Parser p) = Parser (\inp -> [(f v, out) | (v, out) <- p inp])

instance Monad Parser where
   -- return :: a -> Parser a
   return v = Parser (\inp -> [(v,inp)])

   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
   (Parser p) >>= f = Parser (\inp -> concat [papply (f v) out
                                             | (v,out) <- p inp])

instance MonadPlus Parser where
   -- zero :: Parser a
   mzero = Parser (\_ -> [])
   -- (++) :: Parser a -> Parser a -> Parser a
   (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))
       

-- Other primitive parser combinators

       
item :: Parser Char
item = Parser (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

force :: Parser a -> Parser a
force (Parser p) = Parser (\inp -> let x = p inp in
                             (fst (head x), snd (head x)) :  tail x)

first :: Parser a -> Parser a
first (Parser p) = Parser (\inp -> case p inp of
                            []    -> []
                            (x:_) -> [x])

papply :: Parser a -> String -> [(a,String)]
papply (Parser p) inp = p inp
       

-- Derived combinators

       
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `mplus` q)

sat :: (Char -> Bool) -> Parser Char
sat p = do {x <- item; guard (p x); return x}

many :: Parser a -> Parser [a]
many p = force (many1 p +++ return [])

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x  <- p
                    xs <- many (do {sep; p})
                    return(x:xs)

char :: Char -> Parser Char
char x = sat (x==)

alphanum :: Parser Char
alphanum = sat (\c -> isAlphaNum c || c == '@' || c =='\'' )    -- Added @ as a valid character

string :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

hexdigit :: Parser Char
hexdigit = sat isHexDigit
       

--readEnv :: Parser [(String,String)] 
readEnv = (do
          n <- urlEncoded
          string "="
          v <- urlEncoded
          return (n,v)) `sepby` (string "&")

urlEncoded :: Parser String
urlEncoded
 = many ( alphanum `mplus` extra `mplus` safe
         `mplus` do{ char '+' ; return ' '}
         `mplus` do{ char '%'
                   ; d <- hexadecimal
                   ; return $ chr (hex2int d)
                   }
         )

extra :: Parser Char
extra = sat (`elem` "!*'(),")

safe :: Parser Char
safe = sat (`elem` "$-_.")

hexadecimal :: Parser HexString
hexadecimal = do d1 <- hexdigit
                 d2 <- hexdigit
                 return [d1,d2]

type HexString = String

hex2int :: HexString -> Int
hex2int ds = foldl (\n d -> n*16+d) 0 (map (toInt . toUpper) ds)
   where toInt d | isDigit d    =  ord d - ord '0'
         toInt d | isHexDigit d = (ord d - ord 'A') + 10
         toInt d                = error ("hex2int: illegal hex digit " ++ [d])

urlDecode :: String -> [([(String, String)],String)]
urlDecode str=  let Parser p= readEnv in  p str

