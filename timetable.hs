module Timetable (
    minute, stringToMinute, toInterval, interval, stringToInterval, fitsInterval, overlapsInterval,
    schedule, fitsSchedule, overlapsSchedule, stringToSchedule, advise, fromRight
) where

import Data.List
import Data.List.Ordered
import Data.List.Split
import Data.Char (isSpace)
import Control.Monad.Error -- To support (Either String) bind
import Safe
import Data.Maybe

data Minute = Minute Int deriving (Eq, Ord)
minute :: Int -> Either String Minute
minute a    | a < 0 || a > 24*60    = Left "Invalid minute value."
            | otherwise             = Right (Minute a)

hourPart x = show (x `div` 60)
minCorrection x =
    if (length x) == 1
        then "0" ++ x
        else x
minPart x = minCorrection $ show $ x `mod` 60

instance Show Minute where
    show (Minute a) = (hourPart a) ++ ":" ++ (minPart a)

stringToMinute :: String -> Either String Minute
stringToMinute a = let xs = splitOn ":" a in
    if ((length xs) < 2)
        then Left "Minute has no proper hour and minute parts."
        else let hp = liftM (*60) (readMay (xs!!0))
                 mp = readMay $ xs!!1 in
            if ((isNothing hp) || (isNothing mp))
                then Left "There was a problem while parsing the hour or minute part."
                else minute $ (fromJust hp) + (fromJust mp)

-- isLeft (Left _) = True
-- isLeft _        = False

-- Convenience function to convert two ints to an interval.
toInterval :: Int -> Int -> Either String Interval
toInterval a b = do
    aM <- minute a
    bM <- minute b
    interval aM bM

minuteToInt (Minute a) = a
startOf (Interval a b) = minuteToInt a
endOf (Interval a b) = minuteToInt b

-- aStart = a start, aEnd = a end
-- returns true if a fits into b
fitsInterval (Interval aStart aEnd) (Interval bStart bEnd) = aStart >= bStart && aEnd <= bEnd
startsBetween   aStart aEnd bStart bEnd = aStart >= bStart && aStart < bEnd
endsBetween     aStart aEnd bStart bEnd = aEnd > bStart && aEnd <= bEnd
overlapsInterval (Interval aStart aEnd) (Interval bStart bEnd) = startsBetween aStart aEnd bStart bEnd || endsBetween aStart aEnd bStart bEnd

stringToInterval :: String -> Either String Interval
stringToInterval a = let xs = splitOn "-" a in
    if (length(xs) < 2)
        then Left "Interval has no proper start and end."
        else do
            aM <- stringToMinute (xs!!0)
            bM <- stringToMinute (xs!!1)
            interval aM bM

data Interval = Interval Minute Minute
interval :: Minute -> Minute -> Either String Interval
interval a b    | a > b     = Left "Start minute must be smaller than end minute."
                | a == b    = Left "Zero length interval."
                | otherwise = Right $ Interval a b

instance Show Interval where
    show (Interval a b) = (show a) ++ "-" ++ (show b)
instance Eq Interval where
    (Interval aStart aEnd) == (Interval bStart bEnd) = (aStart == bStart) && (aEnd == bEnd)
-- aStartsuming no overlapping intervals. For aStart bStart bEnd aEnd || aStart bStart aEnd bEnd neither ==, <, > will bEnd true ;)
instance Ord Interval where
    (Interval aStart aEnd) < (Interval bStart bEnd) = (aStart < bStart) && (aEnd <= bStart)
    (Interval aStart aEnd) > (Interval bStart bEnd) = (aStart >= bEnd) && (aEnd > bEnd)

-- Very inefficient trim taken from stackoverflow
trim :: String -> String
trim = f . f
	where f = reverse . dropWhile isSpace

data Schedule = Schedule [Interval]
schedule :: [Interval] -> Either String Schedule
schedule is = if (isSorted is) then Right (Schedule is) else Left "Schedule intervals must be ordered."

-- Unfortunately it is O(n) currently
fitsSchedule :: Interval -> Schedule -> Bool
fitsSchedule a (Schedule b) = any (\x -> fitsInterval a x) b

-- Unfortunately it is O(n) currently
overlapsSchedule :: Interval -> Schedule -> Bool
overlapsSchedule a (Schedule b) = any (\x -> overlapsInterval a x) b

-- :t mapM (undefined :: a -> Either String b)
stringToSchedule :: String -> Either String Schedule
stringToSchedule a = mapM stringToInterval (map trim $ splitOn "," a) >>= \is -> Right (Schedule is)

instance Show Schedule where
    show (Schedule a) = intercalate ", " (map show a)

-- closestTo num xs = snd . head . sort $ [(abStart $ num - x, x) | x <- xs]

failed a = case a of
    Left _  -> True
    Right _ -> False

fromRight a = case a of
    Left b  -> error $ show b
    Right b -> b

goodInterval x len opens takens =
    let tI = toInterval x (x + len) in
    if failed tI
        then False
        else (fitsSchedule (fromRight tI) opens) && not (overlapsSchedule (fromRight tI) takens)

snapToGrid x y = x - (x `mod` y)

-- Open Schedule, Taken Schedule, Interval wanted but rejected, minute step
advise :: Schedule -> Schedule -> Interval -> Int -> Either String Interval
advise opens takens i step =
    let start           = snapToGrid (startOf i) step
        xs              = [start, start + step .. 24*60]
        len             = (endOf i) - (startOf i)
    in case find (\x -> goodInterval x len opens takens) xs of
        Just l          -> toInterval l (l + len)
        Nothing         -> Left "No free interval."