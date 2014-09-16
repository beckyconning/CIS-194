module Golf where

import Data.List

skip :: (Int, [(Int, a)]) -> (Int, [(Int, a)])
skip (x, y) = (x, filter (\(z, _) -> z `isMultiple` x) y)

isMultiple :: Int -> Int -> Bool
isMultiple x y = x `mod` y == 0

deindex :: [(Int, a)] -> [a]
deindex x = map (\(_, a) -> a) x

index :: [a] -> [(Int, a)]
index xs = zip [1 .. length xs] xs

tesselate :: [a] -> [[a]]
tesselate x = map (\_ -> x) x

skips :: [a] -> [[a]]
skips = map (deindex) . deindex . map (skip) . index . tesselate . index 
