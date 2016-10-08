module CouchGames.Util (
      shuffle
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