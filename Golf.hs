module Golf where

import Data.List
import Data.Maybe

-- Maps the value of the list over the list (replicates)
hop :: [a] -> [[a]]
hop xs = map (\_ -> xs) xs

-- Returns a tuple containing Just z if the first x is a multiple of y 
step :: (Integer, Integer) -> a -> ((Integer, Integer), Maybe a)
step acc@(x, y) z
    | x `mod` y == 0 = (jump acc, Just z)
    | otherwise      = (jump acc, Nothing)

-- Increments the first integer in a tuple
jump :: (Integer, a) -> (Integer, a)
jump (x, y) = ((x + 1), y)

skip :: Integer -> [a] -> (Integer, [a])
skip x y = (x + 1, (catMaybes . snd . mapAccumL (step) (1, x)) y)

skips :: [a] -> [[a]]
skips = snd . (mapAccumL (skip) 1). hop
