{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import SharedTypes


import LanguageFayJQuery
import LanguageFayPrelude


main= return ()

foo= do --ffi "document.getElementById('tag').innerHTML='hi'"
    this <- getThis -- ffi "document.getElementsByTagName(%1)"

--    text <- getText this
    select "<li></li>" >>= appendTo this >>= setText "hello"
