module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Maybe
import qualified Data.Time as DT
import Text.Printf (printf)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseUtc :: Parser Char Bool
parseUtc = symbol 'Z' *> return True <|> return False

parseYear :: Parser Char Year
parseYear = (\p q r s -> Year $ read [p, q, r, s]) <$> digit <*> digit <*> digit <*> digit

parseMonth :: Parser Char Month
parseMonth = Month <$> digitDigit

parseDay :: Parser Char Day
parseDay = Day <$> digitDigit

parseHour :: Parser Char Hour
parseHour = Hour <$> digitDigit

parseMinute :: Parser Char Minute
parseMinute = Minute <$> digitDigit

parseSecond :: Parser Char Second
parseSecond = Second <$> digitDigit

digitDigit :: Parser Char Int
digitDigit = g <$> digit <*> digit
    where
        g x y = read [x, y]

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p input = listToMaybe allResults
    where
        allResults = map fst $ filter (\(x, y) -> null y) $ parse p input

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime DateTime { date = d, time = t, utc = b } = printDate d ++ "T" ++ printTime t ++ if b then "Z" else ""

printDate :: Date -> String
printDate d = printf "%04d" (runYear (year d)) ++ printf "%02d" (runMonth (month d)) ++ printf "%02d" (runDay (day d))

printTime :: Time -> String
printTime t = printf "%02d" (runHour (hour t)) ++ printf "%02d" (runMinute (minute t)) ++ printf "%02d" (runSecond (second t))

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime DateTime { date = dt, time = ti, utc = u } = isValidDate dt && isValidTime ti
    where
        isValidYear y  = runYear y >= 0
        isValidDate' d = DT.fromGregorianValid (fromIntegral (runYear (year d))) (runMonth (month d)) (runDay (day d))
        isValidDate d  = isJust (isValidDate' d) && isValidYear (year d)
        isValidTime t  = isJust (DT.makeTimeOfDayValid (runHour (hour t)) (runMinute (minute t)) (fromIntegral (runSecond (second t))))