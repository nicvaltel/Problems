module Skiena.Robot.Tree where

import Data.List(delete)

data Tree a b = Node a b [Tree a b]

type Coord = (Double,Double)
type Distance = Double
type PathTree = Tree [Coord] Distance




distance :: Coord -> Coord -> Distance 
distance (x0,y0) (x1,y1) = sqrt ((x1 - x0)^2 + (y1 - y0)^2 )



singleton :: a -> b ->  Tree a b
singleton a b = Node a b []

rootPathTree :: Coord -> PathTree
rootPathTree c = Node [c] 0 []


nodePathTree :: Coord -> [Coord] -> [Coord] -> PathTree
nodePathTree c visitedCs notVisitedCs = 
    let newVisitedCs = c:visitedCs
        d = case visitedCs of
            [] -> 0
            (h:_) -> distance c h
     in Node newVisitedCs d (map (\c' -> nodePathTree c' newVisitedCs (c' `delete` notVisitedCs)) notVisitedCs)
