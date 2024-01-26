module Codewars.Bowling where

type Frames = Int


-- bowlingScore :: [Int] -> Int
-- bowlingScore rolls = bs 1 rolls

-- bs :: Frames -> [Int] -> Int
-- bs 10 rolls = sum rolls
-- bs fr (10:a:b:rest) = 10 + a + b + bs (succ fr) (a:b:rest)
-- bs fr (a:b:c:rest) | a + b == 10 = 10 + c + bs (succ fr) (c:rest)
-- bs fr (a:b:rest) = a + b + bs (succ fr) rest
-- bs _ [x] = error "AAA"
-- bs _ [] = error "BBB"





bowlingScore :: [Int] -> Int
bowlingScore = sum . take 10 . frame
    where
        frame :: [Int] -> [Int]
        frame (10:a:b:rest) = 10 + a + b : frame (a:b:rest)
        frame (a:b:c:rest) | a + b == 10 = 10 + c : frame (c:rest)
        frame (a:b:rest) = a + b : frame rest
        frame _ = error "Incorrect input"







    --   bowlingScore (take 20 $ cycle [0]) `shouldBe` 0
    --   bowlingScore (take 21 $ cycle [9,1]) `shouldBe` 190
    --   bowlingScore (take 12 $ cycle [10]) `shouldBe` 300
    --   bowlingScore [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 10,1,0] `shouldBe` 11
    --   bowlingScore [0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 10, 1,0] `shouldBe` 12