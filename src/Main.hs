{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BC
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import System.Environment
import System.Hardware.Arduino
import System.Hardware.Arduino.SamplePrograms.Morse
import Web.Authenticate.OAuth
import Web.Twitter.Conduit

main :: IO ()
main = do
    putStrLn "Building channels"
    channel <- BC.newBoundedChan channelSize
    putStrLn "Starting Arduino loop"
    _ <- forkIO $ arduinoLoop channel
    putStrLn "Starting Twitter loop"
    _ <- forkIO $ twitterLoop channel
    putStrLn "Running!"
    forever $ threadDelay 1000

channelSize = 10

defaultMessage = "serious emf"

arduinoLoop :: BC.BoundedChan String -> IO ()
arduinoLoop channel = withArduino True "/dev/cu.usbmodemfd131" $ do
    let led = digital outPin
    setPinMode led OUTPUT
    forever $ do
        message <- liftIO $ fmap (fromMaybe defaultMessage) $ BC.tryReadChan channel
        transmit led message

outPin :: Word8
outPin = 12

tokens :: IO OAuth
tokens =
    let
        consumerKey = pack `fmap` getEnv "OAUTH_CONSUMER_KEY"
        consumerSecret = pack `fmap` getEnv "OAUTH_CONSUMER_SECRET"
        t = \x y -> twitterOAuth {oauthConsumerKey = x, oauthConsumerSecret = y}
    in liftM2 t consumerKey consumerSecret

credential :: IO Credential
credential =
    let
        fn = \x y -> Credential
            [ (pack "oauth_token", x)
            , (pack "oauth_token_secret", y)
            ]
        token = pack `fmap` getEnv "OAUTH_TOKEN"
        tokenSecret = pack `fmap` getEnv "OAUTH_TOKEN_SECRET"
    in liftM2 fn token tokenSecret


twInfo :: IO TWInfo
twInfo = liftM3 setCredential tokens credential def

twitterLoop :: BC.BoundedChan String -> IO ()
twitterLoop channel = forever $ do
    threadDelay (1000 * 1000 * 90) -- 1.5 minutes; only allowed 15 requests per 15 minutes (i.e. 1 per minute) so wait 1.5 minutes to definitely avoid this... https://dev.twitter.com/docs/rate-limiting/1.1/limits
    t <- twInfo
    runNoLoggingT . runTW t $ do
        sourceWithMaxId mentionsTimeline
            $= CL.isolate 60
            $$ CL.mapM_ $ \status -> liftIO $ do
                BC.writeChan channel $ T.unpack $ status ^. statusText
