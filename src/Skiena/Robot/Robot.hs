module Skiena.Robot.Robot where

import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.List(delete)



data (Show a, Show b) => Tree a b = Node a b [Tree a b]
    deriving (Show)

type Coord = (Double,Double)
type Distance = Double
type PathTree = Tree [Coord] Distance



distance :: Coord -> Coord -> Distance 
distance (x0,y0) (x1,y1) = sqrt ((x1 - x0)^2 + (y1 - y0)^2 )




singleton :: (Show a, Show b) => a -> b ->  Tree a b
singleton a b = Node a b []

rootPathTree :: Coord -> PathTree
rootPathTree c = Node [c] 0 []


-- nodePathTree :: Coord -> Distance -> [Coord] -> [Coord] -> PathTree
-- nodePathTree c d0 visitedCs notVisitedCs = 
--     let newVisitedCs = c:visitedCs
--         d = case visitedCs of
--             [] -> d0
--             (h:_) -> d0 + distance c h
--      in Node newVisitedCs d (map (\c' -> nodePathTree c' d newVisitedCs (c' `delete` notVisitedCs)) notVisitedCs)


nodePathTree :: Coord -> Distance -> [Coord] -> [Coord] -> PathTree
nodePathTree c d0 visitedCs notVisitedCs = 
    let newVisitedCs = c:visitedCs
        d = case visitedCs of
            [] -> d0
            (h:_) -> d0 + distance c h
     in Node newVisitedCs d (map (\c' -> nodePathTree c' d newVisitedCs (c' `delete` notVisitedCs)) notVisitedCs)



run :: IO ()
run = do
    coords <- readInput "src/Skiena/Robot/input.txt" 
    print coords
    let xxx = nodePathTree (head coords) 0 [] (tail coords)
    print xxx
    pure ()



readInput :: FilePath -> IO [Coord]
readInput file = do
    inputStr <- readFile file
    let coords = flip map (zip (lines inputStr) ([1 .. ] :: [Int])) $ \(s,n) ->
            case readMaybe <$> words s of
                [Just x, Just y] -> (x,y)
                _ -> error (printf "Wrong input data in line #%d '%s' \n in file %s" n s file :: String)
    pure coords



-- completeSearch :: [Coord] -> [Coord]
-- completeSearch cs = undefined


-- nextStep :: ([Coord], Distance) -> [Coord] -> [([Coord], Distance, [Coord])]
-- nextStep ([], _) notVisited = map (\c -> ([c], 0 ,c `delete` notVisited)) notVisited
-- nextStep (visited, prevDist) notVisited = map (\c -> (c:visited, prevDist + distance c (head visited) ,c `delete` notVisited)) notVisited

-- search :: Coord -> [Coord] -> Distance -> 



-- mkPathTree :: Distance -> [Coord] -> PathTree
-- mkPathTree d [c] = PathTree c 0 []
-- mkPathTree (c:rest) = PathTree c 0 []