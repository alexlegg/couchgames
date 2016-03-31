{-# LANGUAGE EmptyDataDecls #-}
module VirtualDOM
    ( h
    , ht
    , diff
    , patch
    , createElement
    , VirtualDom
    , Dom
    ) where

import FFI
import Data.Text

data VTree
data VPatch
data Dom

data VirtualDom = VirtualTree VTree | VirtualText Text

h :: Text -> [(Text, Text)] -> [VirtualDom] -> VirtualDom
h x y z = VirtualTree (h' x y z)

ht :: Text -> VirtualDom
ht = VirtualText

h' :: Text -> [(Text, Text)] -> [VirtualDom] -> VTree
h' = ffi "h_wrapper(%1, %2, %3)"

diff :: VirtualDom -> VirtualDom -> VPatch
diff (VirtualTree x) (VirtualTree y)    = diff' x y
diff _ _                                = error "Cannot call diff on virtual text"

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
