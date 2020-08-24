{-# LANGUAGE OverloadedStrings #-}

module Geek.Parsing where

import Control.Exception(SomeException, catch)

import Data.List.Split(splitOn)
import Data.Maybe(catMaybes, mapMaybe)
import Data.Text(Text, pack, strip)
import Data.Time.Calendar(Day, fromGregorianValid)

import Geek.Data(Event(Birthday), FixedDay(FixedDay), GeekEvent(GeekEvent))

import Network.URI(URI, parseURI)

import System.Directory(doesFileExist, listDirectory)
import System.FilePath(takeBaseName, (</>))

import Text.Read(readMaybe)

_catchHandler :: a -> SomeException -> IO a
_catchHandler = const . pure

catchWithDefault :: a -> IO a -> IO a
catchWithDefault d = (`catch` _catchHandler d)

catchWithNothing :: IO (Maybe a) -> IO (Maybe a)
catchWithNothing = catchWithDefault Nothing

parseDayFromFilename :: FilePath -> Maybe Day
parseDayFromFilename fn = parseDayFromFilename' fn >>= f
    where f i = fromGregorianValid (div i 10000) (fromIntegral (mod (div i 100) 100)) (fromIntegral (mod i 100))

parseDayFromFilename' :: FilePath -> Maybe Integer
parseDayFromFilename' fn
    | (x:_) <- splitOn "-" fn = readMaybe x
    | otherwise = Nothing

wrapGeekEvent :: FilePath -> Event -> IO GeekEvent
wrapGeekEvent fp e = GeekEvent e <$> doesFileExist (fp </> "julian") <*> parseUrls fp <*> parseNotes fp

parseName :: FilePath -> FilePath -> IO (Maybe Text)
parseName root fp = catchWithNothing ((Just . strip . pack) <$> readFile (root </> fp </> "name"))

parseUrls :: FilePath -> IO [URI]
parseUrls fp = catchWithDefault [] (mapMaybe parseURI . lines <$> readFile (fp </> "links"))

parseNotes :: FilePath -> IO Text
parseNotes fp = pure ""

wrapingGeekEvent :: (FilePath -> IO (Maybe Event)) -> FilePath -> FilePath -> IO (Maybe GeekEvent)
wrapingGeekEvent f r n = f n >>= g (wrapGeekEvent (r </> n))
    where g _ Nothing = pure Nothing
          g f (Just x) = Just <$> f x

parseBirthday :: FilePath -> IO (Maybe Event)
parseBirthday fn = do
    name <- parseName "birthdays" fn
    pure (Birthday <$> name <*> fmap FixedDay (parseDayFromFilename fn) <*> pure Nothing)

parseBirthdays :: IO [GeekEvent]
parseBirthdays = do
    ls <- listDirectory "birthdays"
    catMaybes <$> mapM (wrapingGeekEvent parseBirthday "birthdays") ls
