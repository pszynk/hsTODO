module Utils (__PROMPT_SIGN
  , __EPOCH_DAY
  , __TIME_24H
  , date2str
)	where

{-
Funkcje pomocnicze
-}

import Data.Time
import System.Locale

__PROMPT_SIGN :: String
__PROMPT_SIGN = ">"


-- pomocne przy wyliczaniu czasu jako różnicy czasów
__EPOCH_DAY :: UTCTime
__EPOCH_DAY = readTime defaultTimeLocale "%R" "00:00"


-- czas zadania które ma być wykoanne danego dnia
__TIME_24H :: NominalDiffTime
__TIME_24H = diffUTCTime (readTime defaultTimeLocale "%X" "23:59:59") __EPOCH_DAY


date2str :: UTCTime -> String
date2str = formatTime defaultTimeLocale "%F %R"
