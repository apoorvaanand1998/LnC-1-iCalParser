module Main where

import DateTime
import Calendar
import Features
import System.Environment
import System.IO


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = do
  setNewlineTranslations
  mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput = map (run parseDateTime) . lines
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = interact (printOutput . recognizeCalendar)
  where
    printOutput = maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11))

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFileWindows path
  return $ recognizeCalendar string

-- These three functions fight Windows newline translations:
-- without them, on Windows machines, "\r\n" will be read as "\n"
-- and "\n" will be written as "\r\n".
-- Test using these functions rather than "readFile", and parse  newlines as "\r\n",
-- to make sure your parser works on all operating systems (i.e. also for your grader)!
setNewlineTranslations :: IO ()
setNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
readFileWindows :: FilePath -> IO String
readFileWindows p = withBinaryFile p ReadMode hGetContents
writeFileWindows :: FilePath -> String -> IO ()
writeFileWindows p s = withBinaryFile p WriteMode (`hPutStr` s)
