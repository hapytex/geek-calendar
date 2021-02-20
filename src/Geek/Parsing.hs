{-# LANGUAGE OverloadedStrings #-}

module Geek.Parsing where

import Control.Exception(SomeException, catch)

import Data.List.Split(splitOn)
import Data.Maybe(catMaybes, mapMaybe)
import Data.Text(Text, pack, strip)
import Data.Time.Calendar(Day, fromGregorianValid)

import Geek.Data(Event(Birthday), FixedDay(FixedDay), GeekEvent(GeekEvent), Markdown(Markdown), Universe(Universe))

import Network.URI(URI, parseURI)

import System.Directory(doesFileExist, listDirectory)
import System.FilePath((</>))

import Text.Read(readMaybe)

_bddir :: FilePath
_bddir = "birthday"

_undir :: FilePath
_undir = "universe"

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

parseFile :: FilePath -> FilePath -> FilePath -> IO Text
parseFile nm root fp = strip . pack <$> readFile (root </> fp </> nm)

parseToMaybe :: FilePath -> FilePath -> FilePath -> IO (Maybe Text)
parseToMaybe nm root fp = catchWithNothing (Just <$> parseFile nm root fp)

parseToEmpty :: FilePath -> FilePath -> FilePath -> IO Text
parseToEmpty nm root fp = catchWithDefault "" (parseFile nm root fp)

parseName :: FilePath -> FilePath -> IO (Maybe Text)
parseName = parseToMaybe "name"

parseBio :: FilePath -> FilePath -> IO Text
parseBio = parseToEmpty "bio.md"

parseUrls :: FilePath -> IO [URI]
parseUrls fp = catchWithDefault [] (mapMaybe parseURI . lines <$> readFile (fp </> "links"))

parseNotes :: FilePath -> IO Text
parseNotes _ = pure ""

wrapingGeekEvent :: (FilePath -> IO (Maybe Event)) -> FilePath -> FilePath -> IO (Maybe GeekEvent)
wrapingGeekEvent f r n = f n >>= g (wrapGeekEvent (r </> n))
    where g _ Nothing = pure Nothing
          g _f (Just x) = Just <$> _f x

parseBirthday :: FilePath -> IO (Maybe Event)
parseBirthday fn = do
    name <- parseName _bddir fn
    bio <- Markdown <$> parseBio _bddir fn
    pure (Birthday <$> name <*> fmap FixedDay (parseDayFromFilename fn) <*> pure Nothing <*> pure bio)

parseBirthdays :: IO [GeekEvent]
parseBirthdays = do
    ls <- listDirectory _bddir
    catMaybes <$> mapM (wrapingGeekEvent parseBirthday _bddir) ls

parseUniverse :: FilePath -> IO (Maybe Universe)
parseUniverse fn = do
    name <- parseName _undir fn
    urls <- parseUrls (_undir </> fn)
    pure (Universe <$> name <*> pure Nothing <*> pure urls)

parseUniverses :: IO [Universe]
parseUniverses = do
    ls <- listDirectory _undir
    catMaybes <$> mapM parseUniverse ls
