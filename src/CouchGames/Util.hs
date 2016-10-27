module CouchGames.Util (
      shuffle
    , selectRandom
    , updateLast
    , mapFst
    , mapSnd
    ) where

import System.Random
import Control.Monad

shuffle :: [a] -> IO [a]
shuffle xs = foldM shuffle' xs [0..(length xs) - 2]
    where
    shuffle' ys i = do
        j <- getStdRandom (randomR (i, (length xs) - 1))
        return $ swap ys i j

swap :: [a] -> Int -> Int -> [a]
swap xs a b = swap' 0 xs a b
    where
    swap' _ [] _ _ = []
    swap' i (y:ys) a b
        | a == i    = xs !! b : swap' (i+1) ys a b
        | b == i    = xs !! a : swap' (i+1) ys a b
        | otherwise = y : swap' (i+1) ys a b

selectRandom :: [a] -> IO a
selectRandom xs = do
    i <- getStdRandom (randomR (0, (length xs) - 1))
    return (xs !! i)

updateLast :: (a -> a) -> [a] -> [a]
updateLast f (x:[]) = [f x]
updateLast f (x:xs) = updateLast f xs

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
