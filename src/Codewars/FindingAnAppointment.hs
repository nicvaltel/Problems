{-# LANGUAGE ScopedTypeVariables #-}

module Codewars.FindingAnAppointment where
import Text.Read (readMaybe)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

type Minutes = Int
type Duration = Int

beginDay = 9*60
endDay = 19*60

getStartTime :: [[(String, String)]] -> Duration -> Maybe String
getStartTime schedules dur =
    let intervals = map (sortOn fst . map (\(t0,t1) -> (toMinutes t0, toMinutes t1))) schedules
        freeIntervals = 
            flip map intervals $ \intvls ->
                case intvls of
                    [] -> [(beginDay, endDay)]
                    _ -> [(beginDay, fst $ head intvls)] ++ pairMap (\(_,tEnd0) (tBeg1, _) -> (tEnd0,tBeg1)) intvls ++ [(snd $ last intvls, endDay)]
        longFreeIntervals = [ filter (\(tBeg,tEnd) -> tEnd - tBeg >= dur) intvls | intvls <- freeIntervals]
        beg = findIntervalIntersection dur longFreeIntervals
     in fromMinutes <$> beg

pairMap :: (a -> a -> b) -> [a] -> [b]
pairMap f (a0:a1:rest) = f a0 a1 : pairMap f (a1:rest)
pairMap _ [_] = []
pairMap _ [] = []


findIntervalIntersection :: Duration -> [[(Minutes, Minutes)]]-> Maybe Int
findIntervalIntersection _ [] = Nothing
findIntervalIntersection _ [[]] = Nothing
findIntervalIntersection _ [(beg,_):_] = Just beg
findIntervalIntersection dur (sch1:rest) = 
    case foldr (intersectTwoSchedules dur) sch1 rest of
        (beg,_):_ -> Just beg
        _ -> Nothing

intersectIntervalWithSchedule :: Duration -> (Minutes, Minutes) -> [(Minutes, Minutes)] -> [(Minutes, Minutes)]
intersectIntervalWithSchedule dur (beg0, end0) sch =
    flip mapMaybe sch $ \(beg,end) -> 
        if min end end0 - max beg beg0 >= dur
            then Just (max beg beg0, min end end0)
            else Nothing

intersectTwoSchedules :: Duration -> [(Minutes, Minutes)] -> [(Minutes, Minutes)] -> [(Minutes, Minutes)]
intersectTwoSchedules dur sch1 sch2 = concatMap (\int1 -> intersectIntervalWithSchedule dur int1 sch2) sch1

toMinutes :: String -> Minutes
toMinutes str@[h10,h1,':',m10,m1] =
    case (readMaybe [h10] , readMaybe [h1] , readMaybe [m10] , readMaybe [m1]) of
        (Just h10', Just h1' ,Just  m10', Just m1') -> m1' + m10'*10 + h1'*60 + h10'*10*60
        _ -> error $ "Wrong time format in " ++ str
toMinutes str@[h1,':',m10,m1] =
    case (readMaybe [h1] , readMaybe [m10] , readMaybe [m1]) of
        (Just h1' ,Just  m10', Just m1') -> m1' + m10'*10 + h1'*60
        _ -> error $ "Wrong time format in " ++ str
toMinutes str = error $ "Wrong time format in " ++ str

fromMinutes :: Minutes -> String
fromMinutes n = 
    let (h,m) = n `divMod` 60
     in printf "%02d:%02d" h m




testStr :: [[(String, String)]]
testStr = [ [("09:00", "11:30"), ("13:30", "16:00"), ("16:00", "17:30"), ("17:45", "19:00")]
            , [("09:15", "12:00"), ("14:00", "16:30"), ("17:00", "17:30")]
            , [("11:30", "12:15"), ("15:00", "16:30"), ("17:45", "19:00")]
            ]