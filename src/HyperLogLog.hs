module HyperLogLog where

import Data.Vector    (Vector, cons, drop, empty, filter, foldr, maximum,
                       map, maxIndex, reverse, singleton)
import Data.Hashable  (Hashable, hash)
import Prelude hiding (drop, filter, foldr, maximum, map, reverse)
import Text.Read      (readMaybe)

class (Hashable a) => Check a where
    check :: [String] -> Vector a

instance Check Int where
    check [] = empty
    check (x:xs) =
        case readMaybe x :: Maybe Int of
                    Just x  -> cons x (check xs)
                    Nothing -> empty

instance Check Double where
    check [] = empty
    check (x:xs) =
        case readMaybe x :: Maybe Double of
                    Just x  -> cons x (check xs)
                    Nothing -> empty

-- can add more types too, wonder if there's a way
-- to specify the type parameter `a`
-- (readMaybe x :: Maybe `a`) in a normal func ... 
-- wo copy pasting the whole thing repeatedly

hash' :: (Hashable a) => a -> Int
hash' = hash

toBin :: Int -> Vector Int
toBin 0  = singleton 0
toBin x  = reverse $ go x where
    go 0 = empty
    go x = cons (mod x 2) (go (div x 2))

secondLargest :: Vector Int -> Int
secondLargest v = maximum $ filter (/= maximum v) v

hll :: (Hashable a) => Vector a -> Int
hll = secondLargest . countZeroes . map toBin . map hash
    where countZeroes v = foldr (\x acc -> cons (maxIndex (drop 1 x)) acc) empty v