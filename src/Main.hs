
import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BC
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.Word
import System.Hardware.Arduino
import System.Hardware.Arduino.SamplePrograms.Morse

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
