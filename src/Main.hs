
import Control.Concurrent
import Control.Monad
import Data.Word
import System.Hardware.Arduino
import System.Hardware.Arduino.SamplePrograms.Morse

main :: IO ()
main = do
    putStrLn "Starting Arduino loop"
    _ <- forkIO arduinoLoop
    putStrLn "Running!"

arduinoLoop = withArduino True "/dev/cu.usbmodemfd131" $ do
    let led = digital outPin
    setPinMode led OUTPUT
    forever $ transmit led "seriousmorse at emf"

outPin :: Word8
outPin = 12
