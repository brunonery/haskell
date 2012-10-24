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
import qualified Data.ByteString.Char8 as BLC
import qualified Data.Word8 as W
import Web.Authenticate.OAuth
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ConfigFile
import Data.Either.Utils (forceEither)

main :: IO ()
main = do
  conf <- readfile emptyCP "twitter.cfg"
  let cp = forceEither $ conf
  let oauthConsumerKey = BLC.pack $ forceEither $ get cp "oauth" "consumerkey"
  let oauthConsumerSecret = BLC.pack $ forceEither $ get cp "oauth" "consumersecret"
  let oauthAccessToken = BLC.pack $ forceEither $ get cp "oauth" "accesstoken"
  let oauthAccessTokenSecret = BLC.pack $ forceEither $ get cp "oauth" "accesstokensecret"
  let oauth      = newOAuth { oauthConsumerKey = oauthConsumerKey,
                              oauthConsumerSecret = oauthConsumerSecret }
  let credential = newCredential oauthAccessToken oauthAccessTokenSecret
  withManager $ \manager -> do
    request <- parseUrl "http://www.twitter.com/"
    signedRequest <- signOAuth oauth credential request
    Response _ _ _ src <- http signedRequest manager
    -- $$+- instead of $$ because 'src' is a ResumableSource (as per Conduit.hs)
    src C.$$+- conduit C.=$ sinkHandle stdout
  where
    conduit    = CL.map $ B.map W.toUpper
