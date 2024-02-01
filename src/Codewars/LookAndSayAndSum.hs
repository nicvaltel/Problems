module Codewars.LookAndSayAndSum where


lookAndSaySum :: Int -> Int
lookAndSaySum n = sum . last . take n $ iterate conway' [1]

conway :: Int -> [Int] -> [Int]
conway _ [] = []
conway n [i] = [n,i]
conway n (a:b:rest) 
    | a == b = conway (n+1) (b:rest)
    | otherwise = n : a : conway 1 (b:rest)

conway' :: [Int] -> [Int]
conway' (i0:is) = 
    let (iLast,n:rest) = foldr (\i (prevI,n:rest) -> if i == prevI then (i,(n+1):rest) else (i, n:i:rest)) (i0,[1]) is
     in n:iLast:rest

-- toDigits :: Int -> [Int]
-- toDigits n = [read [i] | i <- show n]

-- fromDigits :: [Int] -> Int
-- fromDigits = read . concatMap show