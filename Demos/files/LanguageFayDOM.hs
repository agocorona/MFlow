{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module LanguageFayDOM  where

import Language.Fay.FFI
import Language.Fay.Prelude

data Element
instance Foreign Element

-- | Get body.
getBody :: Fay Element
getBody = ffi "document['body']"

data Timer
instance Foreign Timer

-- | Set a timer.
setTimeout :: Double -> Fay () -> Fay Timer
setTimeout = ffi "window['setTimeout'](%2,%1)"

-- | Set a timer.
setInterval :: Double -> Fay () -> Fay Timer
setInterval = ffi "window['setInterval'](%2,%1)"

-- | Clear a timer.
clearTimeout :: Timer -> Fay ()
clearTimeout = ffi "window['clearTimeout'](%1)"

-- | Alerts.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

documentGetElementById :: String -> Fay Element
documentGetElementById = ffi "document.getElementById(%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"

