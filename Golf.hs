module Golf where

-- Take an indexed list of indexed values and filter out indexed values whose
-- indexes aren't multiples of the list index.
skip :: (Int, [(Int, a)]) -> (Int, [(Int, a)])
skip (x, y) = (x, filter (\(z, _) -> z `isMultiple` x) y)

-- Is x a multiple of y?
isMultiple :: Int -> Int -> Bool
isMultiple x y = x `mod` y == 0

-- Remove the indexes from a list of indexed values.
deindex :: [(Int, a)] -> [a]
deindex x = map (\(_, a) -> a) x

-- Pair each element of an list with an index starting at one.
index :: [a] -> [(Int, a)]
index xs = zip [1 .. length xs] xs

-- Fill the shape of the list with itself.
tesselate :: [a] -> [[a]]
tesselate x = map (\_ -> x) x

-- Index the values in the given list, fill the resulting list with itself and
-- index these lists. This creates the structure 
-- [(ListIndex, [(ValueIndex, value)])]. Filter out indexed values whose indexes
-- aren't multiple of their list index and then deindex the lists and values.
skips :: [a] -> [[a]]
skips = map (deindex) . deindex . map (skip) . index . tesselate . index 
