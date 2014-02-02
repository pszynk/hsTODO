module Utils (__PROMPT_SIGN
  , __EPOCH_DAY
  , __TIME_24H
  , date2str
)	where

import Data.Time
{-import Data.Time.Format-}
import System.Locale

__PROMPT_SIGN :: String
__PROMPT_SIGN = ">"


__EPOCH_DAY :: UTCTime
__EPOCH_DAY = readTime defaultTimeLocale "%R" "00:00"


__TIME_24H :: NominalDiffTime
__TIME_24H = diffUTCTime (readTime defaultTimeLocale "%X" "23:59:59") __EPOCH_DAY


date2str :: UTCTime -> String
date2str = formatTime defaultTimeLocale "%F %R"
