#!/usr/bin/runhaskell
-- This is a ByteString version of match.hs. According to Cedric Staub, this can
-- make things faster. I hope it will :)
import Data.Maybe
import IO
import List
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as B

data Status = Started | Stopped deriving (Enum, Eq, Show)

data Event = Event {
  taskId     :: Int,
  taskName   :: B.ByteString,
  taskStatus :: Status
} deriving (Eq, Show)

getUsefulLines :: [B.ByteString] -> [B.ByteString]
getUsefulLines = filter (=~ "#[0-9]+") . filter (=~ "LTPROBE")

extractEvents :: [B.ByteString] -> [Event]
extractEvents = map extractEvent
  where
    extractId       = fst . fromJust . B.readInt . B.tail . (=~ "#[0-9]+")
    extractName     = B.tail . B.tail . (=~ ": [a-zA-Z]+")
    extractStatus l = if l =~ "Starting" then Started else Stopped
    extractEvent l  = Event{taskId=extractId l, taskName=extractName l, taskStatus=extractStatus l}
   
getUnfinished :: [Event] -> [Event]
getUnfinished = fst . unfinishedAndFinished
  where
    unfinishedAndFinished = foldl matchEvents ([], [])
    matchEvents (started, finished) event
      | taskStatus event == Started = (event:started, finished)
      | taskStatus event == Stopped = (delete event{taskStatus=Started} started, event:finished)

pipeline :: [B.ByteString] -> [B.ByteString]
pipeline = map (B.pack . show) . getUnfinished . extractEvents . getUsefulLines

main :: IO ()
main =
  B.interact (B.unlines . pipeline . B.lines)