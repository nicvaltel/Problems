module Codewars.Lagrange where

lagrange :: (Eq a, Fractional a) => [(a, a)] -> a -> a
lagrange points x = sum [yi * product [(x - xj)/(xi-xj) | xj <- fst <$> points, xj /= xi] | (xi,yi) <- points]


