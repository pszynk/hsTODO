module Repeat
  ( Repeat(..)
  , nextDate
  )	where

{-
Plik z typem reprezentującym cykliczność zadań
-}

import Data.Time (UTCTime, utctDay, addDays, addGregorianMonthsClip, addGregorianYearsClip)

data Repeat = Once | Daily | Weekly | Monthly | Yearly deriving (Eq, Ord, Show, Read)
{--|
ew. dodatkowe typy cykliczności
  CustomPeriod {_repPeriod :: NominalDiffTime} | CustomOccurrence {_repOccurrence :: [UTCTime]} -}

--Wyznacz kolejną datę na podstawie cykliczności
nextDate :: Repeat -> UTCTime -> Maybe UTCTime
nextDate Once _ = Nothing
nextDate Daily x = Just $ x{utctDay = addDays 1 (utctDay x)}
nextDate Weekly x = Just $ x{utctDay = addDays 7 (utctDay x)}
nextDate Monthly x = Just $ x{utctDay = addGregorianMonthsClip 1 (utctDay x)}
nextDate Yearly x = Just $ x{utctDay = addGregorianYearsClip 1 (utctDay x)}
--nextDate _ date = Just date --TODO dla reszty


