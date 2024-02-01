module Codewars.IntegerPartitions where
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- indices :: Int -> Int -> [[Int]]
-- indices n 0 = [replicate n 0]
-- indices n d = nub . concat $ [ map (incrementAt pr) [0 .. n - 1] | pr <- indices n (d - 1) ]

-- incrementAt :: Num a => [a] -> Int -> [a]
-- incrementAt xs i = take i xs ++ [xs !! i + 1] ++ drop (i + 1) xs




-- indices :: Int -> Int -> [[Int]]
-- indices n 0 = [replicate n 0]
-- indices n d = nub $ indices n (d - 1) >>= \xs -> map (incrementAt xs) [0 .. n - 1]

-- incrementAt :: Num a => [a] -> Int -> [a]
-- incrementAt xs i = let (as,b:bs) = splitAt i xs in as ++ [b + 1] ++ bs


-- indices :: Int -> Int -> [[Int]]
indices :: Int -> Int -> [[Int]]
indices n d = Map.elems <$> indices' n d

indices' :: Int -> Int -> [Map Int Int]
indices' n 0 = [Map.fromList $ map (\i -> (i,0)) [1 .. n]]
indices' n d = nub $ indices' n (d - 1) >>= \xs -> map (\i -> Map.adjust (+ 1) i xs) [0 .. n - 1]

-- incrementAt :: Num a => Int -> Map Int a -> Map Int a
-- incrementAt = Map.adjust (+ 1)