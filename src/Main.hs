
import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BC
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (pack)
import Data.Maybe
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
    putStrLn "Running!"

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
