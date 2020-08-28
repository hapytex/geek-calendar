module Main where

import qualified Data.ByteString.Lazy as B
import Data.Default(def)
import Data.Time.Clock(getCurrentTime)

import Geek.Data
import Geek.Parsing(parseBirthdays)

import Text.ICalendar.Printer(printICalendar)

months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

main :: IO ()
main = do
  l <- parseBirthdays
  t <- getCurrentTime
  let c = packCalendar t l
  B.putStr (printICalendar def c)
  putStrLn ""
