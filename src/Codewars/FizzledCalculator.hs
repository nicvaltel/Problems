module Codewars.FizzledCalculator where

-- import Prelude hiding ((*), (/), product)



-- | Should return True if x * y < z, otherwise False.
fizzledCalculator :: Double -> Double -> Double -> Bool
fizzledCalculator x y z =
    case lessSignum (multSignum x y) (round $ signum z) of
            Right b -> b
            Left 1 -> log(abs x) + log(abs y) < log(abs z)
            Left (-1) ->  log(abs x) + log(abs y) > log(abs z)


multSignum :: Double -> Double -> Int
multSignum x y = ms (round $ signum x) (round $ signum y)
    where
    ms :: Int -> Int -> Int
    ms 1 1 = 1
    ms (-1) (-1) = 1
    ms 1 (-1) = -1
    ms (-1) 1 = -1
    ms _ _ = 0

lessSignum :: Int -> Int -> Either Int Bool
lessSignum 1 1 = Left 1
lessSignum (-1) (-1) = Left (-1)
lessSignum x y  = Right (x < y)
