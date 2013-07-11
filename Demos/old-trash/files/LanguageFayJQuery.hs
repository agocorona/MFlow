{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LanguageFayJQuery  where

import Language.Fay.Prelude
import Language.Fay.FFI
import LanguageFayDOM

data JQuery
instance Foreign JQuery
instance Show JQuery

getElementBy :: String -> Fay JQuery
getElementBy = ffi "$('%1')"

getThis :: Fay JQuery
getThis = ffi "this"

addClass :: String -> JQuery -> Fay JQuery
addClass = ffi "%2['addClass'](%1)"

addClassWith :: (Double -> String -> Fay String) -> JQuery -> Fay JQuery
addClassWith = ffi "%2['addClass'](%1)"

getAttr :: String -> JQuery -> Fay String
getAttr = ffi "%2['attr'](%1)"

setAttr :: String -> String -> JQuery -> Fay JQuery
setAttr = ffi "%3['attr'](%1, %2)"

setAttrWith :: String -> (Double -> String -> Fay String) -> JQuery -> Fay JQuery
setAttrWith = ffi "%3['attr'](%1, %2)"

hasClass :: String -> JQuery -> Fay Bool
hasClass = ffi "%2['hasClass'](%1)"

getHtml :: JQuery -> Fay String
getHtml = ffi "%1['html']()"

setHtml :: String -> JQuery -> Fay JQuery
setHtml = ffi "%2['html'](%1)"

setHtmlWith :: (Double -> String -> Fay JQuery) -> JQuery -> Fay JQuery
setHtmlWith = ffi "%2['html'](%1)"

getProp :: String -> JQuery -> Fay String
getProp = ffi "%2['prop'](%1)"

setProp :: String -> String -> JQuery -> Fay JQuery
setProp = ffi "%3['prop'](%1, %2)"

setPropWith :: String -> (Double -> String -> Fay String) -> JQuery -> Fay JQuery
setPropWith = ffi "%3['prop'](%1, %2)"

removeAttr :: String -> JQuery -> Fay JQuery
removeAttr = ffi "%2['removeAttr'](%1)"

removeClass :: String -> JQuery -> Fay JQuery
removeClass = ffi "%2['removeClass'](%1)"

removeClassWith :: (Double -> String -> Fay JQuery) -> JQuery -> Fay JQuery
removeClassWith = ffi "%2['removeClass'](%1)"

removeProp :: String -> JQuery -> Fay JQuery
removeProp = ffi "%2['removeProp'](%1)"

toggleClass :: String -> JQuery -> Fay JQuery
toggleClass = ffi "%2['toggleClass'](%1)"

toggleClassBool :: String -> Bool -> JQuery -> Fay JQuery
toggleClassBool = ffi "%3['toggleClass'](%1, %2)"

toggleAllClasses :: Bool -> JQuery -> Fay JQuery
toggleAllClasses = ffi "%2['toggleClass'](%1)"

toggleClassWith :: (Double -> String -> Bool -> Fay JQuery) -> JQuery -> Fay JQuery
toggleClassWith = ffi "%2['toggleClass'](%1)"

toggleClassBoolWith :: (Double -> String -> Bool -> Fay JQuery) -> Bool -> JQuery -> Fay JQuery
toggleClassBoolWith = ffi "%3['toggleClass'](%1, %2)"

getVal :: JQuery -> Fay String
getVal = ffi "%1['val']()"

setVal :: String -> JQuery -> Fay JQuery
setVal = ffi "%2['val'](%1)"

setValWith :: (Double -> String -> Fay JQuery) -> JQuery -> Fay JQuery
setValWith = ffi "%2['val'](%1)"

setText :: String -> JQuery -> Fay JQuery
setText = ffi "%2['text'](%1)"

setTextWith :: (Double -> String -> Fay JQuery) -> JQuery -> Fay JQuery
setTextWith = ffi "%2['text'](%1)"

getText :: JQuery -> Fay String
getText = ffi "%1['text']()"

holdReady :: Bool -> Fay JQuery
holdReady = ffi "window['jQuery']['holdReady'](%1)"

selectElement :: Element -> Fay JQuery
selectElement = ffi "window['jQuery'](%1)"

select :: String -> Fay JQuery
select = ffi "window['jQuery'](%1)"

selectEmpty :: Fay JQuery
selectEmpty = ffi "window['jQuery']()"

selectQuery :: String -> JQuery -> Fay JQuery
selectQuery = ffi "window['jQuery'](%1, %2)"

ready :: Fay () -> Fay ()
ready = ffi "window['jQuery'](%1)"

getCss :: String -> JQuery -> Fay String
getCss = ffi "%2['css'](%1)"

setCss :: String -> String -> JQuery -> Fay JQuery
setCss = ffi "%3['css'](%1, %2)"

setCssWith :: String -> (Double -> String -> Fay String) -> JQuery -> Fay JQuery
setCssWith = ffi "%3['css'](%1, %2)"

getHeight :: JQuery -> Fay Double
getHeight = ffi "%1['height']()"

setHeight :: Double -> JQuery -> Fay JQuery
setHeight = ffi "%2['height'](%1)"

setHeightWith :: (Double -> Double -> Fay Double) -> JQuery -> Fay JQuery
setHeightWith = ffi "%2['height'](%1)"

getInnerHeight :: JQuery -> Fay Double
getInnerHeight = ffi "%1['innerHeight']()"

getInnerWidth :: JQuery -> Fay Double
getInnerWidth = ffi "%1['innerWidth']()"

getOuterHeight :: JQuery -> Fay Double
getOuterHeight = ffi "%1['outerHeight']()"

getOuterHeightBool :: Bool -> JQuery -> Fay Double
getOuterHeightBool = ffi "%2['outerHeight'](%1)"

getOuterWidth :: JQuery -> Fay Double
getOuterWidth = ffi "%1['outerWidth']()"

getOuterWidthBool :: Bool -> JQuery -> Fay Double
getOuterWidthBool = ffi "%2['outerWidth'](%1)"

getScrollLeft :: JQuery -> Fay Double
getScrollLeft = ffi "%1['scrollLeft']()"

setScrollLeft :: Double -> JQuery -> Fay JQuery
setScrollLeft = ffi "%2['scrollLeft'](%1)"

getScrollTop :: JQuery -> Fay Double
getScrollTop = ffi "%1['scrollTop']()"

setScrollTop :: Double -> JQuery -> Fay JQuery
setScrollTop = ffi "%2['scrollTop'](%1)"

getWidth :: JQuery -> Fay Double
getWidth = ffi "%1['width']()"

setWidth :: Double -> JQuery -> Fay JQuery
setWidth = ffi "%2['width'](%1)"

setWidthWith :: (Double -> Double -> Fay Double) -> JQuery -> Fay JQuery
setWidthWith = ffi "%2['width'](%1)"

after :: JQuery -> JQuery -> Fay JQuery
after = ffi "%2['after'](%1)"

afterWith :: (Double -> Fay JQuery) -> JQuery -> Fay JQuery
afterWith = ffi "%2['after'](%1)"

append :: JQuery -> JQuery -> Fay JQuery
append = ffi "%2['append'](%1)"

appendWith :: (Double -> Fay JQuery) -> JQuery -> Fay JQuery
appendWith = ffi "%2['append'](%1)"

appendTo :: JQuery -> JQuery -> Fay JQuery
appendTo = ffi "%2['appendTo'](%1)"

appendToJQuery :: JQuery -> JQuery -> Fay JQuery
appendToJQuery = ffi "%2['appendTo'](%1)"

before :: JQuery -> JQuery -> Fay JQuery
before = ffi "%2['before'](%1)"

beforeWith :: (Double -> Fay JQuery) -> JQuery -> Fay JQuery
beforeWith = ffi "%2['before'](%1)"

detach :: JQuery -> Fay JQuery
detach = ffi "%1['detach']()"

detachSelector :: String -> JQuery -> Fay JQuery
detachSelector = ffi "%2['detach'](%1)"

empty :: JQuery -> Fay JQuery
empty = ffi "%1['empty']()"

insertAfter :: JQuery -> JQuery -> Fay JQuery
insertAfter = ffi "%2['insertAfter'](%1)"

insertBefore :: JQuery -> JQuery -> Fay JQuery
insertBefore = ffi "%2['insertBefore'](%1)"

prepend :: JQuery -> JQuery -> Fay JQuery
prepend = ffi "%2['prepend'](%1)"

prependWith :: (Double -> Fay JQuery) -> JQuery -> Fay JQuery
prependWith = ffi "%2['prepend'](%1)"

prependTo :: JQuery -> JQuery -> Fay JQuery
prependTo = ffi "%2['prependTo'](%1)"

remove :: JQuery -> Fay JQuery
remove = ffi "%1['remove']()"

removeSelector :: String -> JQuery -> Fay JQuery
removeSelector = ffi "%2['remove'](%1)"

replaceAll :: String -> JQuery -> Fay JQuery
replaceAll = ffi "%2['replaceAll'](%1)"

replaceWith :: String -> JQuery -> Fay JQuery
replaceWith = ffi "%2['replaceWith'](%1)"

replaceWithJQuery :: JQuery -> JQuery -> Fay JQuery
replaceWithJQuery = ffi "%2['replaceWith'](%1)"

replaceWithWith :: (Fay JQuery) -> JQuery -> Fay JQuery
replaceWithWith = ffi "%2['replaceWith'](%1)"

unwrap :: JQuery -> Fay JQuery
unwrap = ffi "%1['unwrap']()"

wrap :: String -> JQuery -> Fay JQuery
wrap = ffi "%2['wrap'](%1)"

wrapWith :: (Double -> Fay JQuery) -> JQuery -> Fay JQuery
wrapWith = ffi "%2['wrap'](%1)"

wrapAllHtml :: String -> JQuery -> Fay JQuery
wrapAllHtml = ffi "%2['wrapAll'](%1)"

wrapAllSelector :: String -> JQuery -> Fay JQuery
wrapAllSelector = ffi "%2['wrapAll'](%1)"

wrapAllElement :: Element -> JQuery -> Fay JQuery
wrapAllElement = ffi "%2['wrapAll'](%1)"

wrapInnerHtml :: String -> JQuery -> Fay JQuery
wrapInnerHtml = ffi "%2['wrapInner'](%1)"

wrapInnerSelector :: String -> JQuery -> Fay JQuery
wrapInnerSelector = ffi "%2['wrapInner'](%1)"

wrapInnerElement :: Element -> JQuery -> Fay JQuery
wrapInnerElement = ffi "%2['wrapInner'](%1)"

addSelector :: String -> JQuery -> Fay JQuery
addSelector = ffi "$2['add'](%1)"

addElement :: Element -> JQuery -> Fay JQuery
addElement = ffi "%2['add'](%1)"

addHtml :: String -> JQuery -> Fay JQuery
addHtml = ffi "%2['add'](%1)"

add :: JQuery -> JQuery -> Fay JQuery
add = ffi "%2['add'](%1)"

addSelectorWithContext :: String -> JQuery -> JQuery -> Fay JQuery
addSelectorWithContext = ffi "%3['add'](%1, %2)"

andSelf :: JQuery -> Fay JQuery
andSelf = ffi "%1['andSelf']()"

children :: JQuery -> Fay JQuery
children = ffi "%1['children']()"

childrenMatching :: String -> JQuery -> Fay JQuery
childrenMatching = ffi "%2['children'](%1)"

closestSelector :: String -> JQuery -> Fay JQuery
closestSelector = ffi "%2['closest'](%1)"

closestWithContext :: String -> String -> JQuery -> Fay JQuery
closestWithContext = ffi "%3['closest'](%1, %2)"

closest :: JQuery -> JQuery -> Fay JQuery
closest = ffi "%2['closest'](%1)"

closestElement :: Element -> JQuery -> Fay JQuery
closestElement = ffi "%2['closest'](%1)"

contents :: JQuery -> Fay JQuery
contents = ffi "%1['contents']()"

each :: (Double -> Element -> Bool) -> JQuery -> Fay JQuery
each = ffi "%2['each'](%1)"

end :: JQuery -> Fay JQuery
end = ffi "%1['end']()"

eq :: Double -> JQuery -> Fay JQuery
eq = ffi "%2['eq'](%1)"

filterWith :: (Double -> Fay Bool) -> JQuery -> Fay JQuery
filterWith = ffi "%2['filter'](%1)"

filterElement :: Element -> JQuery -> Fay JQuery
filterElement = ffi "%2['filter'](%1)"

filterJQuery :: JQuery -> JQuery -> Fay JQuery
filterJQuery = ffi "%2['filter'](%1)"

findSelector :: String -> JQuery -> Fay JQuery
findSelector = ffi "%2['find'](%1)"

findJQuery :: JQuery -> JQuery -> Fay JQuery
findJQuery = ffi "%2['find'](%1)"

findElement :: Element -> JQuery -> Fay JQuery
findElement = ffi "%2['find'](%1)"

first :: JQuery -> Fay JQuery
first = ffi "%1['first']()"

getElement :: JQuery -> Fay Element
getElement = ffi "%1['get'](0)"

has :: String -> JQuery -> Fay JQuery
has = ffi "%2['has'](%1)"

hasElement :: Element -> JQuery -> Fay JQuery
hasElement = ffi "%2['has'](%1)"

is :: String -> JQuery -> Fay JQuery
is = ffi "%2['is'](%1)"

isWith :: (Double -> Bool) -> JQuery -> Fay JQuery
isWith = ffi "%2['is'](%1)"

isJQuery :: JQuery -> JQuery -> Fay JQuery
isJQuery = ffi "%2['is'](%1)"

isElement :: Element -> JQuery -> Fay JQuery
isElement = ffi "%2['is'](%1)"

last :: JQuery -> Fay JQuery
last = ffi "%1['last']()"

jQueryMap :: (Double -> Element -> Fay JQuery) -> JQuery -> Fay JQuery
jQueryMap = ffi "%2['map'](%1)"

next :: JQuery -> Fay JQuery
next = ffi "%1['next']()"

nextSelector :: String -> JQuery -> Fay JQuery
nextSelector = ffi "%2['next'](%1)"

nextAll :: JQuery -> Fay JQuery
nextAll = ffi "%1['nextAll']()"

nextAllSelector :: String -> JQuery -> Fay JQuery
nextAllSelector = ffi "%2['nextAll'](%1)"

nextUntil :: String -> JQuery -> Fay JQuery
nextUntil = ffi "%2['nextUntil'](%1)"

nextUntilFiltered :: String -> String -> JQuery -> Fay JQuery
nextUntilFiltered = ffi "%3['nextUntil'](%1, %2)"

nextUntilElement :: Element -> JQuery -> Fay JQuery
nextUntilElement = ffi "%2['nextUntil'](%1)"

nextUntilElementFiltered :: Element -> String -> JQuery -> Fay JQuery
nextUntilElementFiltered = ffi "%3['nextUntil'](%1, %2)"

isNot :: String -> JQuery -> Fay JQuery
isNot = ffi "%2['not'](%1)"

notElement :: Element -> JQuery -> Fay JQuery
notElement = ffi "%2['not'](%1)"

notElements :: [Element] -> JQuery -> Fay JQuery
notElements = ffi "%2['not'](%1)"

notWith :: (Double -> Bool) -> JQuery -> Fay JQuery
notWith = ffi "%2['not'](%1)"

notJQuery :: JQuery -> JQuery -> Fay JQuery
notJQuery = ffi "%2['not'](%1)"

offsetParent :: JQuery -> Fay JQuery
offsetParent = ffi "%1['offsetParent']()"

parent :: JQuery -> Fay JQuery
parent = ffi "%1['parent']()"

parentSelector :: String -> JQuery -> Fay JQuery
parentSelector = ffi "%2['parent'](%1)"

parents :: JQuery -> Fay JQuery
parents = ffi "%1['parents']()"

parentsSelector :: String -> JQuery -> Fay JQuery
parentsSelector = ffi "%2['parents'](%1)"

parentsUntil :: String -> JQuery -> Fay JQuery
parentsUntil = ffi "%2['parentsUntil'](%1)"

parentsUntilFiltered :: String -> String -> JQuery -> Fay JQuery
parentsUntilFiltered = ffi "%3['parentsUntil'](%1, %2)"

parentsUntilElement :: Element -> JQuery -> Fay JQuery
parentsUntilElement = ffi "%2['parentsUntil'](%1)"

parentsUntilElementFiltered :: Element -> String -> JQuery -> Fay JQuery
parentsUntilElementFiltered = ffi "%3['parentsUntil'](%1, %2)"

prev :: JQuery -> Fay JQuery
prev = ffi "%1['prev']()"

hide :: JQuery -> Fay JQuery
hide = ffi "%1['hide']()"

unhide :: JQuery -> Fay JQuery
unhide = ffi "%1['show']()"

fadeOut :: JQuery -> Fay JQuery
fadeOut = ffi "%1['fadeOut']()"

fadeIn :: JQuery -> Fay JQuery
fadeIn = ffi "%1['fadeIn']()"

focus :: JQuery -> Fay JQuery
focus = ffi "%1['focus']()"

prevSelector :: String -> JQuery -> Fay JQuery
prevSelector = ffi "%2['prev'](%1)"

prevAll :: JQuery -> Fay JQuery
prevAll = ffi "%1['prevAll']()"

prevAllSelector :: String -> JQuery -> Fay JQuery
prevAllSelector = ffi "%2['prevAll'](%1)"

prevUntil :: String -> JQuery -> Fay JQuery
prevUntil = ffi "%2['prevUntil'](%1)"

prevUntilFiltered :: String -> String -> JQuery -> Fay JQuery
prevUntilFiltered = ffi "%3['prevUntil'](%1, %2)"

prevUntilElement :: Element -> JQuery -> Fay JQuery
prevUntilElement = ffi "%2['prevUntil'](%1)"

prevUntilElementFiltered :: Element -> String -> JQuery -> Fay JQuery
prevUntilElementFiltered = ffi "%3['prevUntil'](%1, %2)"

siblings :: JQuery -> Fay JQuery
siblings = ffi "%1['siblings']()"

siblingsSelector :: String -> JQuery -> Fay JQuery
siblingsSelector = ffi "%2['siblings'](%1)"

slice :: Double -> JQuery -> Fay JQuery
slice = ffi "%2['slice'](%1)"

sliceFromTo :: Double -> Double -> JQuery -> Fay JQuery
sliceFromTo = ffi "%3['slice'](%1, %2)"

fin :: a -> Fay ()
fin _ = return ()

size :: JQuery -> Fay Double
size = ffi "%1['length']"

void :: Fay a -> Fay ()
void m = m >> return ()

make :: String -> Fay JQuery
make n = select $ "<" ++ n ++ "></" ++ n ++ ">"

_onKeycode :: (Double -> Fay Bool) -> JQuery -> Fay JQuery
_onKeycode = ffi "%2['keycode'](%1)"

data KeyCode = KeyUp | KeyDown | KeyRet | SomeKey Double
  deriving (Show,Eq)

onKeycode :: (KeyCode -> Fay Bool) -> JQuery -> Fay JQuery
onKeycode callback el = do
  _onKeycode (\code -> callback (case code of
                                   38 -> KeyUp
                                   40 -> KeyDown
                                   13 -> KeyRet
                                   _  -> SomeKey code))
             el

onClick :: Fay Bool -> JQuery -> Fay JQuery
onClick = ffi "%2['click'](%1)"

click :: JQuery -> Fay JQuery
click = ffi "%1['click']()"
