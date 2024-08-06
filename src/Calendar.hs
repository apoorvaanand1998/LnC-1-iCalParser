{-#LANGUAGE OverloadedStrings #-}
module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import Utils
import System.IO
import Data.Maybe ( fromJust, isJust )
import Data.List ( sort )
import qualified Data.Text as T

-- Exercise 6

data Calendar = Calendar { prodId :: String,
                           cEvent :: [Event] }
    deriving (Eq, Ord, Show)

data Event = Event { dStmp    :: DateTime
                   , uid      :: String
                   , dStrt    :: DateTime
                   , dEnd     :: DateTime
                   , desc     :: Maybe String
                   , summary  :: Maybe String
                   , location :: Maybe String }
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = BeginCal -- not necessary but keeping cos advice
           | TProdId { tprodid :: String }
           | TVersion -- same
           | BeginEvent -- same
           | TDStmp { tdstmp :: DateTime }
           | TUid { tuid :: String }
           | TDStrt { tdstrt :: DateTime }
           | TDEnd { tdend :: DateTime }
           | TDesc { tdesc :: String }
           | TSummary { tsumm :: String }
           | TLocation { tloc :: String }
           | EndEvent   -- same
           | EndCal   -- same
           | TCRLF -- same
    deriving (Eq, Ord, Show)
-- keeping a lot of "useless" tokens as per David's advice
-- culling to be done in parsing

scanCalendar :: Parser Char [Token]
scanCalendar = concat <$> sequence [scanBeginCal, scanCalProps, scanEvents, scanEndCal]

scanEvents :: Parser Char [Token]
scanEvents = concat <$> greedy scanEvent'

scanEvent' :: Parser Char [Token]
scanEvent' = sort <$> scanEvent

scanEvent :: Parser Char [Token]
scanEvent = (\x y z -> x ++ y ++ z) <$> scanBeginEvent
                                    <*> scanEventProps
                                    <*> scanEndEvent

-- helper functions required for scanCalendar
-- not putting them inside a where cos testing gets easier

scanBeginCal :: Parser Char [Token]
scanBeginCal = [BeginCal] <$ token "BEGIN:VCALENDAR" <* scanCRLF

scanEndCal :: Parser Char [Token]
scanEndCal = [EndCal] <$ token "END:VCALENDAR" <* scanCRLF

scanProdId :: Parser Char Token
scanProdId = scanEventPropT TProdId "PRODID:"

scanVersion :: Parser Char Token
scanVersion = TVersion <$ token "VERSION:2.0" <* scanCRLF

scanCalProps :: Parser Char [Token]
scanCalProps = sort <$> greedy (choice [scanProdId, scanVersion])

scanBeginEvent :: Parser Char [Token]
scanBeginEvent = [BeginEvent] <$ token "BEGIN:VEVENT" <* scanCRLF

scanEndEvent :: Parser Char [Token]
scanEndEvent = [EndEvent] <$ token "END:VEVENT" <* scanCRLF

scanEventProp :: Parser Char Token
scanEventProp = choice [scanDStmp, scanUid, scanDStrt, scanDEnd,
                        scanDesc, scanSummary, scanLocation]

scanEventProps :: Parser Char [Token]
scanEventProps = greedy scanEventProp

scanCRLF :: Parser Char Token
scanCRLF = TCRLF <$ token "\r\n"

-- For EventProps like {name datetime crlf}
scanEventPropDT :: (DateTime -> Token) -> String -> Parser Char Token
scanEventPropDT f s = f <$> pack (token s) parseDateTime scanCRLF

-- For EventProps like {name text crlf}
scanEventPropT :: (String -> Token) -> String -> Parser Char Token
scanEventPropT f s = f <$> (token s *> scanTextCRLF)

-- 4 functions to make scanning text with CRLF followed by space work
scanTextCRLF :: Parser Char String
scanTextCRLF = (++) <$> scanCRLFSlines <*> scanCRLFline

scanCRLFline :: Parser Char String
scanCRLFline = (++) <$> many (satisfy notCRLF) <*> token "\r\n"

scanCRLFSline :: Parser Char String
scanCRLFSline = (++) <$> many (satisfy notCRLF) <*> token "\r\n "

scanCRLFSlines :: Parser Char String
scanCRLFSlines = concat <$> greedy scanCRLFSline

-- datetime scans
scanDStmp :: Parser Char Token
scanDStmp =  scanEventPropDT TDStmp "DTSTAMP:"

scanDStrt :: Parser Char Token
scanDStrt = scanEventPropDT TDStrt "DTSTART:"

scanDEnd :: Parser Char Token
scanDEnd = scanEventPropDT TDEnd "DTEND:"

-- text scans
scanUid :: Parser Char Token
scanUid = scanEventPropT TUid "UID:"

scanDesc :: Parser Char Token
scanDesc = scanEventPropT TDesc "DESCRIPTION:"

scanSummary :: Parser Char Token
scanSummary = scanEventPropT TSummary "SUMMARY:"

scanLocation :: Parser Char Token
scanLocation = scanEventPropT TLocation "LOCATION:"

-- End of lexing --------------------------------------------------
-- Parsing

parseCalendar :: Parser Token Calendar
parseCalendar = do
    _       <- symbol BeginCal
    prod_id <- parseProdId
    _       <- symbol TVersion
    events  <- greedy parseEvent
    _       <- symbol EndCal
    return $ Calendar prod_id events

parseProdId :: Parser Token String
parseProdId = tprodid <$> satisfy f
    where
        f (TProdId _) = True
        f _           = False

parseEvent :: Parser Token Event
parseEvent = do
    _     <- symbol BeginEvent
    stamp <- parseDStmp
    u_id  <- parseUid
    start <- parseDStrt
    end   <- parseDEnd
    dsc   <- parseDesc
    summ  <- parseSumm
    loc   <- parseLoc
    _     <- symbol EndEvent
    return $ Event stamp u_id start end dsc summ loc

parseDStmp :: Parser Token DateTime
parseDStmp = tdstmp <$> satisfy f
    where
        f (TDStmp _) = True
        f _          = False

parseDStrt :: Parser Token DateTime
parseDStrt = tdstrt <$> satisfy f
    where
        f (TDStrt _) = True
        f _          = False

parseDEnd :: Parser Token DateTime
parseDEnd = tdend <$> satisfy f
    where
        f (TDEnd _) = True
        f _         = False

parseUid :: Parser Token String
parseUid = tuid <$> satisfy f
    where
        f (TUid _) = True
        f _        = False

parseDesc :: Parser Token (Maybe String)
parseDesc = optional $ tdesc <$> satisfy f
    where
        f (TDesc _) = True
        f _         = False

parseSumm :: Parser Token (Maybe String)
parseSumm = optional $ tsumm <$> satisfy f
    where
        f (TSummary _) = True
        f _            = False

parseLoc :: Parser Token (Maybe String)
parseLoc = optional $ tloc <$> satisfy f
    where
        f (TLocation _) = True
        f _             = False

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar c =
  let
    pID = cleanString $ "PRODID:" ++ prodId c
    events = concatMap printEvent (cEvent c)
  in
    "BEGIN:VCALENDAR" ++ clrf ++ pID ++ "VERSION:2.0" ++ clrf ++ events ++ "END:VCALENDAR" ++ clrf

-- Property in the assignment pdf for 8 holds
printCalendarPropertyTest :: FilePath -> IO Bool
printCalendarPropertyTest f = do
    s <- readFile f
    let c = fromJust (recognizeCalendar s)
    putStrLn "A"
    print $ printCalendar c
    putStrLn "B"
    let rc = fromJust $ recognizeCalendar (printCalendar c)
    print $ printCalendar rc
    let pc = printCalendar c
    let prc = printCalendar rc
    let z = filter (uncurry (/=)) (zip pc prc)
    print z
    return $ recognizeCalendar pc == recognizeCalendar prc

insertInside :: Int -> [a] -> [a] -> [a]
insertInside n y xs = countdown n xs where
    countdown 0 xs     = y ++ countdown n xs -- reset to original n
    countdown _ []     = []
    countdown m (x:xs) = x:countdown (m-1) xs

cleanString :: String -> String
cleanString s
    | length s <= 42 = s
    | otherwise      = insertInside 42 "\r\n " (T.unpack (T.concat (filter (/= "") (map T.strip (T.splitOn "\r\n" (T.pack s)))))) ++ clrf

clrf :: String
clrf = "\r\n"

printEvent :: Event -> String
printEvent e =
  let
    be    = "BEGIN:VEVENT" ++ clrf
    dtStp = "DTSTAMP:" ++ printDateTime (dStmp e) ++ clrf
    uID   = cleanString $ "UID:" ++ uid e
    dtStr = "DTSTART:" ++ printDateTime (dStrt e) ++ clrf
    dtEnd = "DTEND:" ++ printDateTime (dEnd e) ++ clrf
    des   = cleanString (maybe "" ("DESCRIPTION:" ++) (desc e))
    smry  = cleanString (maybe "" ("SUMMARY:" ++) (summary e))
    loc   = cleanString (maybe "" ("LOCATION:" ++) (location e))
    end   = "END:VEVENT" ++ clrf
  in
    be ++ dtStp ++ uID ++ dtStr ++ dtEnd ++ des ++ smry ++ loc ++ end

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFile path
  return $ recognizeCalendar string  