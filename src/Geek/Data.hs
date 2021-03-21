{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Geek.Data where

import Data.Char(isAlphaNum)
import Data.Default(def)
import Data.Foldable(toList)
import Data.List(intersperse)
import qualified Data.Map.Strict as M
import Data.Set(empty, fromList)
import Data.Text(Text, isSuffixOf, pack)
import qualified Data.Text as T
import Data.Text.Lazy(fromStrict)
import qualified Data.Text.Lazy as L
import Data.Time.Calendar(Day, toGregorian)
import Data.Time.Calendar.Julian(toJulian)
import Data.Time.Clock(UTCTime(UTCTime))

import Network.URI(URI, parseURI)

import Text.Blaze.Html(Html, toHtml)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import Text.Blaze.Html5(b, br, i, p)
import Text.Blaze.Internal(MarkupM(Parent))
import Text.ICalendar.Types(
    Categories(Categories), Comment(Comment), Created(Created), Date(Date), DateTime(UTCDateTime), Description(Description), DTStamp(DTStamp)
  , DTStart(DTStartDate, DTStartDateTime), EventStatus(ConfirmedEvent), FBType(Free), Frequency(Yearly), Language(Language), LastModified(LastModified)
  , RRule(RRule), Recur(Recur), Summary(Summary), TimeTransparency(Transparent), UID(UID), URL(URL), VCalendar(vcEvents)
  , VEvent(
      VEvent, veAlarms, veAttach, veAttendee, veCategories, veClass, veComment, veContact, veCreated, veDescription, veDTEndDuration, veDTStamp
    , veDTStart, veExDate, veGeo, veLastMod, veLocation, veOrganizer, veOther, vePriority, veRDate, veRecurId, veRelated, veResources, veRRule
    , veRStatus, veSeq, veSummary, veStatus, veTransp, veUID, veUrl
    )
  , Weekday(Monday)
  )
import Text.Markdown(markdown)

_fbType :: FBType
_fbType = Free

_language :: Maybe Language
_language = Just (Language "EN")

_projectLink :: Maybe URI
_projectLink = parseURI "https://github.com/hapytex/geek-calendar"

_eventComment :: Comment
_eventComment = Comment "Generated by geek-calendar" _projectLink _language def

toPossessive :: Text -> Text
toPossessive t
    | "s" `isSuffixOf` t = t <> "'"
    | otherwise = t <> "'s"

newtype FixedDay = FixedDay Day deriving (Eq, Ord, Show)
newtype Markdown = Markdown Text deriving (Eq, Ord, Show)

data Universe
  = Universe { universeName :: Text, universeEmoji :: Maybe Text, universeLinks :: [URI] }
  deriving (Eq, Ord, Show)

data Event
  = Birthday { person :: Text, birthDay :: FixedDay, deathday :: Maybe Day, bio :: Markdown }  -- birthday is the event
  | FixedEvent { eventName :: Text, eventDescription :: Markdown, whyDay :: Markdown, eventUniverse :: Maybe Universe, eventDay :: FixedDay }
  deriving (Eq, Ord, Show)

data GeekEvent
  = GeekEvent { event :: Event, julian :: Bool, links :: [URI], notes :: Text }
  deriving (Eq, Ord, Show)

class Describe a where
    describe' :: a -> Html
    describe :: a -> L.Text
    describe = renderHtml . describe'

class Footnotes a where
    footnotes' :: a -> [Text]
    footnotes' = const []
    footnotes :: a -> Html
    footnotes = foldr ((<>) . Parent "fn" "<fn" "</fn>" . toHtml) "" . footnotes'

class ToRecur a where
    toRecur :: a -> Recur
    toRRule :: a -> RRule
    toRRule = (`RRule` def) . toRecur
    startTime' :: a -> UTCTime
    startTime :: a -> DTStamp
    startTime = (`DTStamp` def) . startTime'
    dtStart :: a -> DTStart
    dtStart = (`DTStartDateTime` def) . UTCDateTime . startTime'

class ToCategories a where
    toCategories :: a -> [Text]

class ToUniqueIdentifier a where
    toUniqueIdentifier' :: a -> Text
    toUniqueIdentifier :: a -> UID
    toUniqueIdentifier = (`UID` def) . fromStrict . toUniqueIdentifier'

class ToSummary a where
    toSummary' :: a -> Text
    toEmoji :: a -> [Text]
    toEmoji = const []
    toSummary :: a -> Summary
    toSummary x = Summary (fromStrict (toSummary' x <> ems)) Nothing _language def
        where ems | xa@(_:_) <- toEmoji x = " " <> T.concat xa
                  | otherwise = ""

ordinal :: Integral i => i -> Text
ordinal n | rem (div n 10) 10 == 1 = "th"
          | otherwise = go' (rem n 10)
    where go' 1 = "st"
          go' 2 = "nd"
          go' 3 = "rd"
          go' _ = "th"

monthName :: Int -> Text
monthName 1 = "January"
monthName 2 = "February"
monthName 3 = "March"
monthName 4 = "April"
monthName 5 = "May"
monthName 6 = "June"
monthName 7 = "July"
monthName 8 = "August"
monthName 9 = "September"
monthName 10 = "October"
monthName 11 = "November"
monthName 12 = "December"
monthName _ = error "The month name does not exists."

formatDate :: Integer -> Int -> Int -> Text
formatDate y m d = monthName m <> " " <> pack (show d) <> ordinal d <> ", " <> pack (show y)

julianNote :: Text -> Day -> Text
julianNote nm dy = "At the time of " <> nm <> ", the Julian calendar was in place, and the date was " <> formatDate y m d <> "."
    where (y, m, d) = toJulian dy

instance Describe Day where
    describe' dy = toHtml (formatDate y m d)
        where (y, m, d) = toGregorian dy

instance Describe FixedDay where
    describe' (FixedDay d) = b (describe' d)

instance Describe Event where
    describe' Birthday { person=_p, birthDay=bd, bio=_bio } = describe' bd <> ": " <> i (toHtml (toPossessive _p)) <> " Birthday." <> describe' _bio
    describe' FixedEvent { eventName=_n, eventDay=_ed, eventDescription=_ds } = describe' _ed <> ": " <> toHtml _n <> ". " <> describe' _ds

instance Describe GeekEvent where
    describe' g@GeekEvent { event=ev, links=urls } = describe' ev <> footnotes g <> printUris urls

instance Describe Markdown where
    describe' (Markdown md) = markdown def (fromStrict md)

instance Footnotes GeekEvent where
    footnotes' g@GeekEvent {julian=j}
        | j, UTCTime d 0 <- startTime' g = [julianNote (toSummary' g) d]
        | otherwise = []

instance ToRecur FixedDay where
    toRecur (FixedDay _) = Recur Yearly Nothing 1 [] [] [] [] [] [] [] [] [] Monday
    startTime' (FixedDay d) = UTCTime d 0
    dtStart (FixedDay d) = DTStartDate (Date d) def

instance ToRecur Event where
    toRecur Birthday { birthDay=bd } = toRecur bd
    toRecur FixedEvent { eventDay=ed } = toRecur ed
    startTime' Birthday { birthDay=bd } = startTime' bd
    startTime' FixedEvent { eventDay=ed } = startTime' ed
    dtStart Birthday { birthDay=bd } = dtStart bd
    dtStart FixedEvent { eventDay=ed } = dtStart ed

instance ToRecur GeekEvent where
    toRecur GeekEvent {event=ev} = toRecur ev
    startTime' GeekEvent {event=ev} = startTime' ev
    dtStart GeekEvent {event=ev} = dtStart ev

instance ToCategories Universe where
    toCategories Universe { universeName=un } = [un]

instance ToCategories Event where
    toCategories Birthday { person=_p } = [_p, "birthday"]
    toCategories FixedEvent { eventUniverse = Just u} = toCategories u

instance ToCategories GeekEvent where
    toCategories GeekEvent { event=e } = toCategories e

instance ToSummary Universe where
  toSummary' Universe { universeName=un } = un
  toEmoji Universe { universeEmoji=em } = toList em

instance ToSummary Event where
    toSummary' Birthday { person=_p } = toPossessive _p <> " Birthday"
    toSummary' FixedEvent { eventName=en, eventUniverse=un } = go un en
      where go Nothing = id
            go (Just Universe {universeName=u}) = (<> (" (" <> u <> ")"))
    toEmoji Birthday {} = ["\x1f382"]
    toEmoji FixedEvent { eventUniverse=Just un } = toEmoji un
    toEmoji FixedEvent {} = []

instance ToSummary GeekEvent where
    toSummary' GeekEvent { event=e } = toSummary' e
    toEmoji GeekEvent { event=e } = toEmoji e
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
    toUniqueIdentifier' Birthday { person=_p, birthDay=bd } = toUniqueIdentifier' bd <> "-" <> toUniqueIdentifier' _p
    toUniqueIdentifier' FixedEvent { eventName=_en, eventDay=_ed } = toUniqueIdentifier' _ed <> "-" <> toUniqueIdentifier' _en

instance ToUniqueIdentifier GeekEvent where
    toUniqueIdentifier GeekEvent {event=e} = toUniqueIdentifier e
    toUniqueIdentifier' GeekEvent {event=e} = toUniqueIdentifier' e

urisToUrl :: [URI] -> Maybe URL
urisToUrl (x:_) = Just (URL x def)
urisToUrl _ = Nothing

printUris :: [URI] -> Html
printUris [] = ""
printUris ls = p (mconcat (intersperse br (map (toHtml . pack . show) ls)))

_geekCategories :: [Text] -> Categories
_geekCategories ts = Categories (fromList ("geek" : map fromStrict ts)) _language def

toEvent :: UTCTime -> GeekEvent -> ((L.Text, Maybe (Either Date DateTime)), VEvent)
toEvent utc g@GeekEvent { links=l } = ((fromStrict (toUniqueIdentifier' g), Nothing), VEvent {
    veAlarms=empty
  , veAttach=empty
  , veAttendee=empty
  , veCategories=[_geekCategories (toCategories g)]
  , veClass=def
  , veComment=[_eventComment]
  , veContact=empty
  , veCreated=Just (Created utc def)
  , veDTEndDuration=Nothing
  , veDTStart=Just (dtStart g)
  , veDTStamp=startTime g
  , veDescription=Just (Description (describe g) Nothing _language def)
  , veExDate=empty
  , veGeo=Nothing
  , veLastMod=Just (LastModified utc def)
  , veLocation=Nothing
  , veOrganizer=Nothing
  , veOther=empty
  , vePriority=def
  , veRDate=empty
  , veRecurId=Nothing
  , veRelated=empty
  , veResources=empty
  , veRRule=[toRRule g]
  , veRStatus=empty
  , veSeq=def
  , veStatus=Just (ConfirmedEvent def)
  , veSummary=Just (toSummary g)
  , veTransp=Transparent def
  , veUID=toUniqueIdentifier g
  , veUrl=urisToUrl l
  })

packCalendar :: UTCTime -> [GeekEvent] -> VCalendar
packCalendar utc gs = def {
    vcEvents=M.fromList (map (toEvent utc) gs)
  }
