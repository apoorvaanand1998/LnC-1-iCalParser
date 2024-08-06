module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes
import qualified Data.Time as T
import qualified Data.Bifunctor as DB
import Data.Maybe
import Text.Printf

-- Exercise 9
countEvents :: Calendar -> Int
countEvents c = length (cEvent c)

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt c = foldr (\x r -> if dt >= dStrt x && dt < dEnd x then x:r else r) [] (cEvent c)

checkOverlapping :: Calendar -> Bool
checkOverlapping c = foldr (\x r -> length (findEvents (dStrt x) c) > 1 || r) False (cEvent c)

timeSpent :: String -> Calendar -> Int
timeSpent sum c = foldr (\e r -> r + calcTimeDifference (dEnd e) (dStrt e)) 0 filteredE
    where
        filteredE = filter (\e -> case summary e of
            Nothing -> False
            Just a  -> sum == a) (cEvent c)

calcTimeDifference :: DateTime -> DateTime -> Int
calcTimeDifference (DateTime dd1 t1 _) (DateTime dd2 t2 _) = floor (toRational $ T.diffUTCTime utc1 utc2 / 60)
    where
        days :: Date -> T.Day
        days x = T.fromGregorian ((toInteger . runYear . year) x) ((runMonth . month) x) ((runDay . day) x)
        day1  = days dd1
        day2  = days dd2
        diffTime x = T.secondsToDiffTime . toInteger $ (runHour . hour) x * 3600 + (runMinute . minute) x * 60 + (runSecond . second) x
        diffTime1  = diffTime t1
        diffTime2  = diffTime t2
        utc1 = T.UTCTime day1 diffTime1
        utc2 = T.UTCTime day2 diffTime2

-- Exercise 10

-- Events lasting multiple days are shown by starting anew
-- so something from 15 november 13:00 -> 16 november 15:00 would create
-- 15 november 13:00 - 15 november 23:59 and 16 november 0:00 - 16 november 15:00

-- use this function to print out the boxes
-- ppMonth's string only works with putStrLn
pp :: FilePath -> (Int, Int) -> IO () -- filepath_of_calendar -> (year, month)
pp f (y, m) = do
    c <- readCalendar f
    putStrLn $ ppMonth (Year y) (Month m) (fromJust c)

ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = render box
  where
    maxDay = daysaMonth y m
    evts   = findAllEventsMonth y m (cEvent c) -- gets all the cEvent that happen that month
    iv :: [(Int, [Event])]
    iv = createDateEventsMapping y m evts 1 maxDay -- gets (Int, [event]) for on what day, what cEvent happen
    is :: [(Int, [String])]
    is = ieTois iv -- converts the [event] to a [string] we can print in the boxes
    box = oneMonthBox maxDay is

daysaMonth :: Year -> Month -> Int
daysaMonth y m = T.gregorianMonthLength ((toInteger . runYear) y) (runMonth m)

findAllEventsMonth :: Year -> Month -> [Event] -> [Event]
findAllEventsMonth y m = filter (checkInMonth y m)

createDateEventsMapping :: Year -> Month -> [Event] -> Int -> Int -> [(Int, [Event])]
createDateEventsMapping _ _ _ current maxDayCount
    | current > maxDayCount = [] -- zip [1..maxDayCount] (repeat [])
createDateEventsMapping y m evts current maxDayCount =
  let
    (todayEvt, newEvt) = foldr (\e (r, r2)-> let (a, b) = eventSplit y m e current maxDayCount in (a++r, b++r2)) ([], []) evts
  in
    (current, todayEvt) : createDateEventsMapping y m newEvt (current + 1) maxDayCount

-- for a day and an event, checks if that event started and ended on that day, result is (happening this day, happens on another day)
-- if an event lasts multiple days, we create two new cEvent, one that ends today, one that starts tomorrow
eventSplit :: Year -> Month -> Event -> Int -> Int -> ([Event], [Event])
eventSplit y m evt current maxDayCount
    | started && ended = ([evt], []) -- event happens today, not another day
    | started && not ended = ([dtFinal], dtNext) -- event starts today, ends another day
    | otherwise = ([], [evt])
  where
    started = hasEventStarted y m current evt
    ended = hasEventEnded y m current evt
    dtNext = createNewEventDtStart evt maxDayCount  -- from the start of the next day 0:00:00, until the end of the event
    dtFinal = createNewEventDtEnd evt -- from the start of the event, until 23:59:59


-- Checks if Event e has started on Year-Month-Int
-- d will be changing from 1..maxDayCount
hasEventStarted :: Year -> Month -> Int -> Event -> Bool
hasEventStarted y m d e =
  let
    eStartTime = dStrt e
    eDate      = date eStartTime
    currDate   = Date y m (Day d)
  in
    eDate == currDate

--checks if event has ended
hasEventEnded :: Year -> Month -> Int -> Event -> Bool
hasEventEnded y m d e =
  let
    eEnd       = dEnd e
    checkingDT = DateTime (Date y m (Day d)) (Time (Hour 23) (Minute 59) (Second 59)) True
  in
    eEnd <= checkingDT

-- creates from an event, an event that has the dEnd on the same date, but with time being 23:59:59
createNewEventDtEnd :: Event -> Event
createNewEventDtEnd e =
  let
    dtEd  = dEnd e
    newTime   = Time (Hour 23) (Minute 59) (Second 59) -- new time
    newDtEnd  = dtEd { time = newTime } -- new date time
  in
    e { dEnd = newDtEnd } -- new event

--creates from an event (if it's still in the month), a new event that happens next day and starts at 0:00:00 instead
createNewEventDtStart :: Event -> Int -> [Event]
createNewEventDtStart e maxDayCount =
  let
    eDayPlusOne = 1 + runDay (day (date (dStrt e))) -- new day integer
    newDtStart  = dStrt e
    newDay      = Day eDayPlusOne
    newTime     = Time (Hour 0) (Minute 0) (Second 0)
    newDt       = (date (dStrt e)) { day = newDay } 
    newDT       = DateTime newDt newTime True 
    newEvent    = e { dStrt = newDT } 
  in
    ([newEvent | eDayPlusOne <= maxDayCount])

checkInMonth :: Year -> Month -> Event -> Bool
checkInMonth y m e = (year . date) eStart <= y ||
                     (year . date) eStart == y && (month .date) eStart <= m -- if start is before or in this month
  where
    eStart = dStrt e
    eEnd   = dEnd e

ieTois :: [(Int,[Event])] -> [(Int, [String])]
ieTois = map (DB.second mapping)
  where
    mapping :: [Event] -> [String]
    mapping = map (\e -> timeToSpecialString ((time . dStrt) e) ((time . dEnd) e))

timeToSpecialString :: Time -> Time -> String
timeToSpecialString (Time h1 m1 _) (Time h2 m2 _) = printHour h1 ++ ":" ++ printMinute m1 ++ " - " ++
                                                    printHour h2 ++ ":" ++ printMinute m2
    where
        printHour t   = printf "%02d" (runHour t)
        printMinute t = printf "%02d" (runMinute t)

defaultString :: String
defaultString = "--------------"

stringToBox :: [String] -> Box
stringToBox = foldr (\x r -> vcat center1 [text x, r]) nullBox

--from cEvent to a box

oneMonthBox :: Int -> [(Int, [String])] -> Box
oneMonthBox maxDay is = vcat left listBox
  where
    fillis = is ++ replicate (42 - maxDay) (42, [])
    (week1Box, week1String) = oneWeekOverHead maxDay fillis
    (week2Box, week2String) = oneWeekOverHead maxDay week1String
    (week3Box, week3String) = oneWeekOverHead maxDay week2String
    (week4Box, week4String) = oneWeekOverHead maxDay week3String
    (week5Box, week5String) = oneWeekOverHead maxDay week4String -- fills it out eventually
    (week6Box,_) = oneWeekOverHead maxDay week5String -- literally just for the bottom line
    defaultBoxes =  [week1Box, week2Box, week3Box, week4Box, week5Box]
    listBox | maxDay == 28 = defaultBoxes -- week 6 isn't needed if there are only 28 days
            | otherwise = defaultBoxes ++ [week6Box] -- week 6 is needed

-- from one week of cEvent, to a box for a week
oneWeekOverHead :: Int -> [(Int, [String])] -> (Box, [(Int, [String])])
oneWeekOverHead maxDay ss = (punctuateH left interLeaveBox boxes, drop 7 ss)
  where
   boxes = oneWeekBox maxDay (take 7 ss)
   heightBox :: Int -- max height of the week
   heightBox = foldr (max . rows) 0 boxes
   interLeaveBox = addBorder heightBox 

-- all boxes for a  day in a week, in a list
oneWeekBox :: Int -> [(Int, [String])] -> [Box]
oneWeekBox maxDay = map (oneDayBox maxDay)

-- a box for a day
oneDayBox :: Int -> (Int, [String]) -> Box
oneDayBox maxDay (i, ss) | i > maxDay = text defaultString
                         | otherwise = vcat center1 [base, rest]
  where
    base = text defaultString // text (show i)
    rest = stringToBox ss

-- adds the | at the border of the boxes
addBorder :: Int -> Box
addBorder 0 = nullBox
addBorder i = char '|' // addBorder (i-1)