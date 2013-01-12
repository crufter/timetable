module TimetableTest where

import Test.HUnit
import Timetable

e1 (x,_,_) = x
e2 (_,x,_) = x
e3 (_,_,x) = x

want what verdict = assertBool (show what) verdict

cases1 = [
    ("8:00-9:00", "10:00-11:00", False),
    ("8:00-9:00", "8:50-9:20", True),
    ("8:50-9:20", "8:00-9:00", True),
    ("8:00-10:00", "10:00-11:00", False),
    ("10:00-11:00", "8:00-10:00", False)
    ]
test1' c =  let i1 = fromRight (stringToInterval (e1 c))
                i2 = fromRight (stringToInterval (e2 c)) in
            overlapsInterval i1 i2
test1 = TestCase $ mapM_ (\c -> want c (e3 c == test1' c)) cases1

cases2 = [
    ("8:00-9:00", "8:00-9:00", True),
    ("8:00-9:00", "8:00-10:00", True),
    ("8:00-9:00", "7:00-9:00", True),
    ("8:00-9:00", "8:01-9:00", False),
    ("8:00-9:00", "8:00-8:59", False),
    ("8:00-9:00", "8:01-8:59", False)
    ]
test2' c =
    let i1 = fromRight (stringToInterval (e1 c))
        i2 = fromRight (stringToInterval (e2 c)) in
    fitsInterval i1 i2
test2 = TestCase $ mapM_ (\c -> want c (e3 c == test2' c)) cases2

-- ((open, taken), (wanted, step), check)
cases3 = [
    (("8:00-12:00, 14:00-17:30", "8:00-9:30, 10:30-12:00"), ("9:00-9:30", 30), "9:30-10:00"),
    (("8:00-12:00, 14:00-17:30", "8:00-8:20"),              ("8:00-8:30", 30), "8:30-9:00")
    ]
test3' c =
    let open = fromRight $ stringToSchedule ((fst . e1) c)
        taken = fromRight $ stringToSchedule ((snd . e1) c)
        wanted = fromRight $ stringToInterval ((fst . e2) c)
        step = (snd . e2) c in
    case (advise open taken wanted step) of
        Left x -> show x
        Right y -> show y
test3 = TestCase $ mapM_ (\c -> let res = test3' c in want ((show c) ++ " result: " ++ res) ((e3 c) == res)) cases3

tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3
    ]