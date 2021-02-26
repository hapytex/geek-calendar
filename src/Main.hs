module Main where

import qualified Data.ByteString.Lazy as B
import Data.Default(def)
import Data.Time.Clock(getCurrentTime)

import Geek.Data
import Geek.Parsing(parseBirthdays, parseUniverses)

import Text.ICalendar.Printer(printICalendar)

main :: IO ()
main = do
  us <- parseUniverses
  l <- parseBirthdays
  t <- getCurrentTime
  let c = packCalendar t l
  B.putStr (printICalendar def c)
  putStrLn ""
