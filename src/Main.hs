module Main where

import Geek.Data
import Geek.Parsing(parseBirthdays)

months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

main :: IO ()
main = do
  l <- parseBirthdays
  print l
