module Geek.Data where

import Data.Default(def)
import Data.Text(Text)
import Data.Time.Calendar(Day)

import Network.URI(URI)

import Text.ICalendar.Types(
    FBType(Free), Frequency(Yearly), RRule(RRule), Recur(Recur)
  , TimeTransparency(Transparent), URL(URL)
  , VEvent(VEvent, veClass, veTransp, veUrl)
  , Weekday(Monday)
  )

_fbType :: FBType
_fbType = Free

data Event
  = Birthday { person :: Text, deathday :: Maybe Day }  -- birthday is the event
  deriving (Eq, Ord, Show)

newtype FixedDay = FixedDay Day deriving (Eq, Ord, Show)

class ToRecur a where
    toRecur :: a -> Recur
    toRRule :: a -> RRule
    toRRule = (`RRule` def) . toRecur

instance ToRecur FixedDay where
    toRecur (FixedDay d) = Recur Yearly Nothing 1 [] [] [] [] [] [] [] [] [] Monday

data GeekEvent
  = GeekEvent { event :: Event, day :: Day, julian :: Bool, links :: [URI], notes :: [Text] }
  deriving (Eq, Ord, Show)

urisToUrl :: [URI] -> Maybe URL
urisToUrl (x:_) = Just (URL x def)
urisToUrl _ = Nothing

toCalendar :: GeekEvent -> VEvent
toCalendar GeekEvent { links=l } = VEvent { veClass=def, veTransp=Transparent def, veUrl=urisToUrl l }
