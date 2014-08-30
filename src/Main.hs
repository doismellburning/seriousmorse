
import Control.Concurrent
import qualified Control.Concurrent.BoundedChan as BC
import Control.Monad
import Control.Monad.Trans (liftIO)
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

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ x = x

defaultMessage = "serious emf"

arduinoLoop :: BC.BoundedChan String -> IO ()
arduinoLoop channel = withArduino True "/dev/cu.usbmodemfd131" $ do
    let led = digital outPin
    setPinMode led OUTPUT
    forever $ do
        message <- liftIO $ liftM3 if' (BC.isEmptyChan channel) (return defaultMessage) (BC.readChan channel)
        transmit led message

outPin :: Word8
outPin = 12
