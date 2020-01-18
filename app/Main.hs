module Main where

import HyperLogLog

import Data.Vector  as V   (Vector, fromList, map)


main :: IO ()
main = do
    print "..."
    a <- inps
    print $ hll (check a :: V.Vector Double)
    -- print $ V.map toBin $ V.map hash' $ V.fromList ([1.0 .. 10.0] :: [Double])
    print $ hll $ V.fromList ([1.0 .. 10.0] :: [Double])

inps :: IO [String]
inps = do
    x <- getLine
    if x == "q" then return [] else do
        y <- inps
        return (x:y)

printCardinality :: Int -> IO ()
printCardinality x = print ("the estimated cardinality of the set is " ++ show (2^x) ++ ".")