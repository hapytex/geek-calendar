{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Geek.Data where

import Data.Char(isAlphaNum)
import Data.Default(def)
import Data.Set(empty, fromList)
import Data.Text(Text, isSuffixOf, pack)
import qualified Data.Text as T
import Data.Text.Lazy(fromStrict)
import Data.Time.Calendar(Day, toGregorian)
import Data.Time.Clock(UTCTime)

import Network.URI(URI)

import Text.ICalendar.Types(
    Categories(Categories), Created(Created), EventStatus(ConfirmedEvent), FBType(Free), Frequency(Yearly), Language(Language), LastModified(LastModified), RRule(RRule), Recur(Recur)
  , Summary(Summary)
  , TimeTransparency(Transparent), UID(UID), URL(URL)
  , VEvent(VEvent, veAlarms, veAttach, veAttendee, veCategories, veClass, veCreated, veGeo, veLastMod, veLocation, veOrganizer, veOther, vePriority, veRelated, veResources, veRRule, veSummary, veStatus, veTransp, veUID, veUrl)
  , Weekday(Monday)
  )

_fbType :: FBType
_fbType = Free

_language :: Maybe Language
_language = Just (Language "EN")

toPossessive :: Text -> Text
toPossessive t
    | isSuffixOf "s" t = t <> "'"
    | otherwise = t <> "'s"

newtype FixedDay = FixedDay Day deriving (Eq, Ord, Show)

data Event
  = Birthday { person :: Text, birthDay :: FixedDay, deathday :: Maybe Day }  -- birthday is the event
  deriving (Eq, Ord, Show)

data GeekEvent
  = GeekEvent { event :: Event, julian :: Bool, links :: [URI], notes :: Text }
  deriving (Eq, Ord, Show)

class ToRecur a where
    toRecur :: a -> Recur
    toRRule :: a -> RRule
    toRRule = (`RRule` def) . toRecur

class ToCategories a where
    toCategories :: a -> [Text]

class ToUniqueIdentifier a where
    toUniqueIdentifier' :: a -> Text
    toUniqueIdentifier :: a -> UID
    toUniqueIdentifier = (`UID` def) . fromStrict . toUniqueIdentifier'

class ToSummary a where
    toSummary' :: a -> Text
    toSummary :: a -> Summary
    toSummary x = Summary (fromStrict (toSummary' x)) Nothing _language def

instance ToRecur FixedDay where
    toRecur (FixedDay d) = Recur Yearly Nothing 1 [] [] [] [] [] [] [] [] [] Monday

instance ToRecur Event where
    toRecur Birthday { birthDay=bd } = toRecur bd

instance ToRecur GeekEvent where
    toRecur GeekEvent {event=ev} = toRecur ev

instance ToCategories Event where
    toCategories Birthday { person=p } = [p]

instance ToCategories GeekEvent where
    toCategories GeekEvent { event=e } = toCategories e

instance ToSummary Event where
    toSummary' Birthday { person=p } = toPossessive p <> " Birthday"

instance ToSummary GeekEvent where
    toSummary' GeekEvent { event=e } = toSummary' e
    toSummary GeekEvent { event=e } = toSummary e

instance ToUniqueIdentifier Day where
    toUniqueIdentifier' dy = pack (show (10000 * y + 100 * fromIntegral m + fromIntegral d))
        where (y, m, d) = toGregorian dy

instance ToUniqueIdentifier FixedDay where
    toUniqueIdentifier' (FixedDay d) = toUniqueIdentifier' d

instance ToUniqueIdentifier Text where
    toUniqueIdentifier' = T.map f
        where f c | isAlphaNum c = c
                  | otherwise = '-'

instance ToUniqueIdentifier Event where
    toUniqueIdentifier' Birthday { person=p, birthDay=bd } = toUniqueIdentifier' bd <> "-" <> toUniqueIdentifier' p

instance ToUniqueIdentifier GeekEvent where
    toUniqueIdentifier GeekEvent {event=e} = toUniqueIdentifier e
    toUniqueIdentifier' GeekEvent {event=e} = toUniqueIdentifier' e

urisToUrl :: [URI] -> Maybe URL
urisToUrl (x:_) = Just (URL x def)
urisToUrl _ = Nothing

_geekCategories :: [Text] -> Categories
_geekCategories ts = Categories (fromList ("geek" : map fromStrict ts)) _language def

toEvent :: UTCTime -> GeekEvent -> VEvent
toEvent utc (g@GeekEvent { links=l }) = VEvent {
    veAlarms=empty
  , veAttach=empty
  , veAttendee=empty
  , veCategories=[_geekCategories (toCategories g)]
  , veClass=def
  , veCreated=Just (Created utc def)
  , veGeo=Nothing  -- TODO: can be spec'ed
  , veLastMod=Just (LastModified utc def)
  , veLocation=Nothing  -- TODO: can be spec'ed
  , veOrganizer=Nothing  -- TODO: can be spec'ed
  , veOther=empty
  , vePriority=def
  , veRelated=empty  -- TODO: can be spec'ed
  , veResources=empty
  , veRRule=[toRRule g]
  , veStatus=Just (ConfirmedEvent def)
  , veSummary=Just (toSummary g)
  , veTransp=Transparent def
  , veUID=toUniqueIdentifier g
  , veUrl=urisToUrl l
  }

-- packCalendar :: 
