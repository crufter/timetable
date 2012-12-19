import Data.List
import Data.List.Split
import Data.Char (isSpace)

hourPart x = show (x `div` 60)
minCorrection x =
    if (length x) == 1
        then "0" ++ x
        else x
minPart x = minCorrection $ show $ x `mod` 60

data Minute = Minute Int deriving (Eq, Ord)
minute :: Int -> Minute
minute a    | a < 0 || a > 24*60    = error "Invalid minute value."
            | otherwise             = Minute a
instance Show Minute where
    show (Minute a) = (hourPart a) ++ ":" ++ (minPart a)

stringToMinute :: String -> Minute
stringToMinute a = let xs = splitOn ":" a in minute $ (read $ xs!!0) * 60 + (read $ xs!!1)

-- isLeft (Left _) = True
-- isLeft _        = False

-- Convenience function to convert two ints to an interval fa1t.
toInterval :: Int -> Int -> Interval
toInterval a b = Interval (minute a) (minute b)

minuteToInt (Minute a) = a
startOf (Interval a b) = minuteToInt a
endOf (Interval a b) = minuteToInt b

-- a1 = a start, a2 = a end
-- returns true if a fits into b
fitsInterval (Interval a1 a2) (Interval b1 b2) = a1 >= b1 && a2 <= b2
overlapsInterval (Interval a1 a2) (Interval b1 b2) = (a1 > b1 && a1 < b2) || (a2 > b1 && a2 < b2) || (b1 > a1 && b1 < a2) || (b2 > a1 && b2 < a2)

stringToInterval a =
    let xs = splitOn "-" a
    in interval (stringToMinute (xs!!0)) (stringToMinute (xs!!1))

data Interval = Interval Minute Minute
interval a b    | a > b     = error "Start minute must b2 smaller then end minute."
                | a == b    = error "Zero length interval."
                | otherwise = Interval a b

instance Show Interval where
    show (Interval a b) = (show a) ++ "-" ++ (show b)
instance Eq Interval where
    (Interval a1 a2) == (Interval b1 b2) = (a1 == b1) && (a2 == b2)
-- a1suming no overlapping intervals. For a1 b1 b2 a2 || a1 b1 a2 b2 neither ==, <, > will b2 true ;)
instance Ord Interval where
    (Interval a1 a2) < (Interval b1 b2) = (a1 < b1) && (a2 <= b1)
    (Interval a1 a2) > (Interval b1 b2) = (a1 >= b2) && (a2 > b2)

-- Very inefficient trim taken from stackoverflow
trim :: String -> String
trim = f . f
	where f = reverse . dropWhile isSpace

data Schedule = Schedule [Interval]

-- Unfortunately it is O(n) currently
fitsSchedule :: Interval -> Schedule -> Bool
fitsSchedule a (Schedule b) = any (\x -> fitsInterval a x) b

-- Unfortunately it is O(n) currently
overlapsSchedule :: Interval -> Schedule -> Bool
overlapsSchedule a (Schedule b) = any (\x -> overlapsInterval a x) b

stringToSchedule :: String -> Schedule
stringToSchedule a = Schedule $ map stringToInterval $ map trim $ splitOn "," a

instance Show Schedule where
    show (Schedule a) = intercalate ", " (map show a)

-- closestTo num xs = snd . head . sort $ [(ab1 $ num - x, x) | x <- xs]

-- Open Schedule, Taken Schedule, Interval wanted but rejected, minute step
advise :: Schedule -> Schedule -> Interval -> Int -> Interval
advise open taken i step =
    let snap            = round $ (startOf i) / step
        xs              = [snap, snap + step .. 24*60]
        len             = (endOf i) - (startOf i)
        intervalize x   = toInterval x (x + len)
    in case (find (\x -> (fitsSchedule (intervalize x) open) && (not (overlapsSchedule (intervalize x) taken))) xs) of
        Just l          -> toInterval l step
        Nothing         -> error "No free interval."