{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LanguageFayJQueryCookie where

import Language.Fay.Prelude
import Language.Fay.FFI

data Cookie a = Cookie
  { cookieName    :: String
  , cookieValue   :: a
  , cookieExpires :: Maybe Double
  , cookiePath    :: Maybe String
  }

-- | Set a session cookie.
setSessionCookie :: String -> String -> Fay ()
setSessionCookie = ffi "jQuery['cookie'](%1,JSON.stringify(%2))"

-- | Delete a cookie.
deleteCookie :: String -> Fay ()
deleteCookie = ffi "jQuery['removeCookie'](%1)"

-- | Get cookie value.
getCookie :: String -> Fay (Maybe String)
getCookie key = do
  exists <- cookieExists key
  if exists
     then do value <- _cookieValue key
             return (Just value)
     else return Nothing

-- | Check a cookie exists.
cookieExists :: String -> Fay Bool
cookieExists = ffi "jQuery['cookie'](%1) !== null"

-- | Get a cookie's value.
_cookieValue :: String -> Fay String
_cookieValue = ffi "jQuery['cookie'](%1)"
