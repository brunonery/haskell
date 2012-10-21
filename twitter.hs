#!/usr/bin/runhaskell
-- Twitter: baby steps towards a library for handling the streaming API.
-- Someone implemented this (in Japanese): http://goo.gl/TZOlI
-- I also should read about Haskell conduits: http://goo.gl/5emJ9
-- And maybe about parser combinators.
-- The next step is to try and make this conduit stream. A good test is to pipe
-- the output to grep and see if it rolls the output or spits everything at once.
import Char
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.Word8 as W

main =
  simpleHttp "http://www.twitter.com" >>=
  (return . L.pack . map W.toUpper . L.unpack) >>=
  L.putStr
