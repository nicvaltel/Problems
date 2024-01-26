module Codewars.ProcessTree where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)

type PID = Int
data Process = Process PID [Process]
    deriving (Show)

data ProcessPIDS = ProcessPIDS PID [PID]

rootPid :: PID
rootPid = 1

makeTree :: [(PID, PID)] -> Process
makeTree xs =
    let mapPPIDS = foldr appendProcess (Map.singleton rootPid (ProcessPIDS rootPid [])) (tail $ sortOn snd xs)
    in mkProcess 1 mapPPIDS

    where
        appendProcess :: (PID, PID) -> Map PID ProcessPIDS -> Map PID ProcessPIDS
        appendProcess (pid, ppid) m =
            case Map.lookup ppid m of
                Just (ProcessPIDS _ ps) -> Map.insert ppid (ProcessPIDS ppid (pid:ps)) m
                Nothing -> Map.insert ppid (ProcessPIDS ppid [pid]) m

        mkProcess :: PID -> Map PID ProcessPIDS -> Process
        mkProcess pid m =
            case Map.lookup pid m of
                Just (ProcessPIDS _ ps) -> Process pid (map (`mkProcess` m) ps)
                Nothing -> Process pid []


processes :: [(Int, Int)]
processes = [
    (1, -1),
    (219, 214),
    (214, 1),
    (124,1)
  ]

--   Process 1 [Process 214 [Process 219 []] , Process 124 []]