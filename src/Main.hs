
import Control.Monad
import Data.Word
import System.Hardware.Arduino
import System.Hardware.Arduino.SamplePrograms.Morse

main :: IO ()
main = withArduino True "/dev/cu.usbmodemfd131" $ do
    let led = digital outPin
    setPinMode led OUTPUT
    forever $ transmit led "seriousmorse at emf"

outPin :: Word8
outPin = 12
