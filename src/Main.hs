module Main where

import qualified Data.ByteString.Lazy as B
import Data.Default.Class(def)
import Data.Time.Clock(getCurrentTime)

import Geek.Data
import Geek.Parsing(parseBirthdays, parseFixedEvents, parseUniverses)

import Text.ICalendar.Printer(printICalendar)

main :: IO ()
main = do
  uv <- parseUniverses
  l <- parseBirthdays
  fe <- parseFixedEvents uv
  t <- getCurrentTime
  let c = packCalendar t (l ++ fe)
  B.putStr (printICalendar def c)
