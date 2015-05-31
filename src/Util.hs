module Util where

-- Take every nth element from a list.
takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:xs) = x : (takeEvery n $ drop (n-1) xs)

-- zipWith, padding lists with a specified element when elements of one
-- of the lists run out.
zipWithPadding :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithPadding f p [] ys = zipWith f (repeat p) ys
zipWithPadding f p xs [] = zipWith f xs (repeat p)
zipWithPadding f p (x:xs) (y:ys) = (f x y) : zipWithPadding f p xs ys

-- zipWithPadding, preprending the second list with n padding elements
zipWithOffset :: (a -> a -> b) -> a -> Int -> [a] -> [a] -> [b]
zipWithOffset f p n xs ys = zipWithPadding f p xs $ (replicate n p) ++ ys
