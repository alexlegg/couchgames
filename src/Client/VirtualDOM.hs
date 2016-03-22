{-# LANGUAGE EmptyDataDecls #-}
module VirtualDOM
    ( h
    , diff
    , patch
    , createElement
    , VirtualDom(..)
    , VTree
    , Dom
    ) where

import FFI

data VTree
data VPatch
data Dom

data VirtualDom = VirtualTree VTree | VirtualText String

h :: String -> [(String, String)] -> [VirtualDom] -> VTree
h = ffi "h_wrapper(%1, %2, %3)"

diff :: VTree -> VTree -> VPatch
diff = ffi "require('virtual-dom').diff(%1, %2)"

patch :: Dom -> VPatch -> Fay Dom
patch = ffi "require('virtual-dom').patch(%1, %2)"

createElement :: VTree -> Fay Dom
createElement = ffi "require('virtual-dom').create(%1)"
