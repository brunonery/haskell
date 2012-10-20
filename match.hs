#!/usr/bin/runhaskell
-- This script process the standard input, looking for lines that have both
-- LTPROBE and # followed by a number. Then it converts each line to an event
-- (task started or stopped) and finds all the tasks that started but did not
-- stop. It can serve as a template for text-processing scripts.
import IO
import List
import Text.Regex.Posix

data Status = Started | Stopped deriving (Enum, Eq, Show)

data Event = Event {
  taskId     :: Int,
  taskName   :: String,
  taskStatus :: Status
} deriving (Eq, Show)

getUsefulLines :: [String] -> [String]
getUsefulLines = filter (=~ "#[0-9]+") . filter (=~ "LTPROBE")

extractEvents :: [String] -> [Event]
extractEvents = map extractEvent
  where
    extractId       = read . tail . (=~ "#[0-9]+")
    extractName     = tail . tail . (=~ ": [a-zA-Z]+")
    extractStatus l = if l =~ "Starting" then Started else Stopped
    extractEvent l  = Event{taskId=extractId l, taskName=extractName l, taskStatus=extractStatus l}
   
getUnfinished :: [Event] -> [Event]
getUnfinished = fst . unfinishedAndFinished
  where
    unfinishedAndFinished = foldl matchEvents ([], [])
    matchEvents (started, finished) event
      | taskStatus event == Started = (event:started, finished)
      | taskStatus event == Stopped = (delete event{taskStatus=Started} started, event:finished)

pipeline :: [String] -> [String]
pipeline = map show . getUnfinished . extractEvents . getUsefulLines

main :: IO ()
main =
  interact (unlines . pipeline . lines)
