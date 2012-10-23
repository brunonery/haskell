#!/usr/bin/runhaskell
-- Twitter: baby steps towards a library for handling the streaming API.
-- Someone implemented this (in Japanese): http://goo.gl/TZOlI
-- I also should read about Haskell conduits: http://goo.gl/5emJ9
-- And maybe about parser combinators.
-- The next step is to try and make this conduit stream. A good test is to pipe
-- the output to grep and see if it rolls the output or spits everything at once.
import Data.Conduit.Binary (sinkHandle)
import IO (stdout)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.Word8 as W

main :: IO()
main = do
  request <- parseUrl "http://www.twitter.com/"
  withManager $ \manager -> do
    Response _ _ _ src <- http request manager
    -- $$+- instead of $$ because 'src' is a ResumableSource (as per Conduit.hs)
    src C.$$+- conduit C.=$ sinkHandle stdout
      where
        conduit = CL.map $ B.map W.toUpper
