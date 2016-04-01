{-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
module VirtualDOM
    ( VirtualDom
    , VirtualProp
    , Event
    , Dom
    , initDom
    , updateDom

    , h
    , ht
    , hdiv
    , hspan
    , table
    , tr
    , td
    , input
    , inputType
    , value
    , colspan
    , onClick
    ) where

import Prelude
import FFI
import Data.Text

-- Virtual DOM API

data VTree
data VPatch
data Dom
data Event

data VirtualDom = VirtualTree VTree | VirtualText Text

data VirtualProp =
      TextProp Text Text
    | EventListener Text (Event -> Fay ())

h :: Text -> [VirtualProp] -> [VirtualDom] -> VirtualDom
h x y z = VirtualTree (h' x y z)

ht :: Text -> VirtualDom
ht = VirtualText

h' :: Text -> [VirtualProp] -> [VirtualDom] -> VTree
h' = ffi "h_wrapper(%1, %2, %3)"

diff :: VirtualDom -> VirtualDom -> VPatch
diff x y = diff' (forceToVTree x) (forceToVTree y)

forceToVTree :: VirtualDom -> VTree
forceToVTree (VirtualTree x)    = x
forceToVTree x                  = forceToVTree (hspan [] [x])

diff' :: VTree -> VTree -> VPatch
diff' = ffi "require('virtual-dom').diff(%1, %2)"

patch :: Dom -> VPatch -> Fay Dom
patch = ffi "require('virtual-dom').patch(%1, %2)"

createElement :: VirtualDom -> Fay Dom
createElement (VirtualTree x) = createElement' x
createElement (VirtualText x) = createElementText x

createElement' :: VTree -> Fay Dom
createElement' = ffi "require('virtual-dom').create(%1)"

createElementText :: Text -> Fay Dom
createElementText = ffi "require('virtual-dom').create(%1)"

-- Actual DOM API

getVTree :: Fay VirtualDom
getVTree = ffi "window.virtualTree"

putVTree :: VirtualDom -> Fay ()
putVTree = ffi "window.virtualTree = %1"

newRootNode :: Dom -> Fay ()
newRootNode = ffi "window.rootNode = document.body.appendChild(%1)"

putRootNode :: Dom -> Fay ()
putRootNode = ffi "window.rootNode = %1"

getRootNode :: Fay Dom
getRootNode = ffi "window.rootNode"

initDom :: (a -> VirtualDom) -> a -> Fay ()
initDom render s = do
    let t = render s
    putVTree t
    dom <- createElement t
    newRootNode dom

updateDom :: (a -> VirtualDom) -> a -> Fay ()
updateDom render s = do
    t       <- getVTree
    let t'  = render s
    let p   = diff t t'
    root    <- getRootNode
    root'   <- patch root p
    putRootNode root'
    putVTree t'

-- Convenience functions

hdiv :: [VirtualProp] -> [VirtualDom] -> VirtualDom
hdiv = h "div"

hspan :: [VirtualProp] -> [VirtualDom] -> VirtualDom
hspan = h "span"

table :: [VirtualProp] -> [VirtualDom] -> VirtualDom
table = h "table"

tr :: [VirtualProp] -> [VirtualDom] -> VirtualDom
tr = h "tr"

td :: [VirtualProp] -> [VirtualDom] -> VirtualDom
td = h "td"

input :: [VirtualProp] -> VirtualDom
input prop = h "input" prop []

inputType :: Text -> VirtualProp
inputType = TextProp "type"

value :: Text -> VirtualProp
value = TextProp "value"

colspan :: Text -> VirtualProp
colspan = TextProp "colspan"

onClick :: (Event -> Fay ()) -> VirtualProp
onClick = EventListener "onclick"
