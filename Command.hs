module Command(Command, CommandHandler, runCommand) where


import qualified Data.Map as Map
import Task

import System.Exit (exitSuccess)

import System.Console.ANSI (clearScreen
  , setCursorPosition)
  --, Color(..)
  --, ColorIntensity(..)
  --, ConsoleLayer(..)
  --, SGR(..)
  --, Underlining(..)
  --, setSGRCode)

import Control.Monad.Trans()
import Data.Time
import Data.Time.Clock()
import Data.Time.Format()
import System.Locale -- zastapic
import Control.Monad.State.Strict

import System.Console.Haskeline ()

import Data.Maybe
import System.IO
import Control.Exception()
import Logic
import Utils
import Repeat

import Control.Applicative
import Text.Read as TR
import Data.List as DL

data Command = Command {_cmdKey :: String
 , _cmdFormat :: String
 , _cmdInfo :: String
 , _cmdHandler :: CommandHandler
 }

type SuccessMsg = Maybe String
type FailureMsg = String
type CommandStatus = Either FailureMsg SuccessMsg

type CommandHandler = [String] -> StateWithIO CommandStatus

cmdList :: [Command]
cmdList =
  [Command {_cmdKey="quit", _cmdFormat="[]",      _cmdInfo="quit program",    _cmdHandler=handleQuit}
  ,Command {_cmdKey="clear", _cmdFormat="[]",      _cmdInfo="clear terminal",  _cmdHandler=handleClear}
  ,Command {_cmdKey="new",  _cmdFormat="[title]", _cmdInfo="create new task", _cmdHandler=handleNew}
  ,Command {_cmdKey="list", _cmdFormat="[]",      _cmdInfo="list all tasks",  _cmdHandler=handleList}
  ,Command {_cmdKey="cdate", _cmdFormat="[]",      _cmdInfo="change date",  _cmdHandler=handleCdate}
  ,Command {_cmdKey="ctime", _cmdFormat="[]",      _cmdInfo="change time",  _cmdHandler=handleList}
  ]

cmdMap :: Map.Map String Command
cmdMap = Map.fromList $ map (\cmd -> (_cmdKey cmd, cmd)) cmdList

runCommand :: CommandHandler
runCommand [] = return $ Left "nie podano komendy"
runCommand line@(kcmd:args) = case Map.lookup kcmd cmdMap of
  Nothing -> return $ Left $ "Zła komenda: " ++ unwords line-- liftIO $ putStrLn $ "Zła komenda: " ++ unwords line
  Just cmd -> _cmdHandler cmd args

handleQuit :: CommandHandler
handleQuit _ = do
  b <- liftIO $ _boolQuestion "Zamknąć program?" False
  when b $ liftIO exitSuccess
  return $ Right Nothing


handleClear :: CommandHandler
handleClear _ = do
  cl <- liftIO $ _boolQuestion "Czyscic?" True
  when cl $ liftIO _clearScreen
  return $ Right Nothing

handleNew :: CommandHandler
handleNew [] = return $ Left "Podaj tytuł nowego zadania"
handleNew xs = do
  liftIO $ putStrLn  "Tworzenie nowego zadania"
  let title = unwords xs
  liftIO $ putStrLn $ "Tytuł: '" ++ title ++ "'"
  minfo <- liftIO _infoQuestion
  today <- liftIO getToday
  mdate <- liftIO $ _dateQuestion today __TIME_24H
  if isNothing mdate
  then return $ Left "Przerwano tworzenie zadania"
  else do
    let Just date = mdate
    liftIO $ putStrLn $ "Data zadania: " ++ date2str date --maybe "" date2str mdate
    mrepeat <- liftIO _repeatQuestion
    if isNothing mrepeat
    then return $ Left "Przerwano tworzenie zadania"
    else do
      let Just rrepeat = mrepeat
      task <- createNextTask title minfo rrepeat date
      liftIO $ putStrLn "Tworzenie zadania:"
      liftIO $ print $ show task
      ifadd <- liftIO $ _boolQuestion "Potwierdz nowe zadanie" True
      if ifadd
      then do 
        addNextTask task
        return $ Right $ Just "Dodano nowe zadanie"
      else return $ Left "Odrzucono nowe zadanie"

handleList ::  CommandHandler
handleList _ = return $ Right Nothing

handleCdate ::  CommandHandler
handleCdate (datestr:_) = do
  let mdate = parseTime defaultTimeLocale "%F" datestr
  case mdate of
    Nothing -> return $ Left $ "Zły format daty: " ++ datestr
    Just date -> do
      setTime date
      return $ Right $ Just "Zmieniono datę"

_clearScreen :: IO ()
_clearScreen = do
  clearScreen
  setCursorPosition 0 0


_boolQuestion :: String -> Bool -> IO Bool
_boolQuestion x deflt = do
  putStr question
  hFlush stdout
  line <- getLine
  case line of
      "y" -> return True
      "n" -> return False
      ""  -> return deflt
      _   -> _boolQuestion x deflt
  where
    question = x ++ ":" ++ options ++ __PROMPT_SIGN ++ " "
    options = "(" ++ yes ++ "|" ++ no ++ ")"
    (yes, no)  = if deflt
      then ("[" ++ "y" ++ "]", "n")
      else ("y", "[" ++ "n" ++ "]")

_appendTillNNL :: Int -> Int -> [String] -> IO [String]
_appendTillNNL 0 _ _ = return []
_appendTillNNL n k xs
  | n < k = error "zle uzycie funkcji"
  | n == k = return xs
  | otherwise = do
    line <- getLine
    if null line
    then _appendTillNNL n (k+1) xs
    else _appendTillNNL n 0 (line:xs)
      --where
        --xss = xs ++ (take k $ repeat "\n")

_parseDay :: String -> Maybe UTCTime
_parseDay day = parseTime defaultTimeLocale "%F" day

_parseTime :: String -> Maybe NominalDiffTime
_parseTime time = pure diffUTCTime <*> (parseTime defaultTimeLocale "%R" time) <*> Just  __EPOCH_DAY

_parseDate :: [String] -> UTCTime -> NominalDiffTime -> Maybe UTCTime
_parseDate [] defDay defTime =
  Just $ addUTCTime defTime defDay
_parseDate [dayOrTime] defDay defTime =
  onlyDay `mplus` onlyTime
  where
    onlyDay = pure addUTCTime <*> Just defTime <*> mDay
    onlyTime = pure addUTCTime <*> mTime <*> Just defDay
    mDay = _parseDay dayOrTime
    mTime = _parseTime dayOrTime
_parseDate (dayOrTime1:dayOrTime2:_) _ _ =
  dayAndTime1 `mplus` dayAndTime2
  where
    dayAndTime1 = pure addUTCTime <*> mTime1 <*> mDay2
    dayAndTime2 = pure addUTCTime <*> mTime2 <*> mDay1
    mDay1 = _parseDay dayOrTime1
    mTime1 = _parseTime dayOrTime1
    mDay2 = _parseDay dayOrTime2
    mTime2 = _parseTime dayOrTime2

_dateQuestion :: UTCTime -> NominalDiffTime -> IO (Maybe UTCTime)
_dateQuestion defDay defTime = do
  putStr $ "Podaj datę zadania [" ++ date2str defDate ++ "]: "
  hFlush stdout
  line <- getLine
  let mDate = _parseDate (words line) defDay defTime
  if isNothing mDate
  then do
    putStrLn "Błędna data"
    stop <- _boolQuestion "Chcesz przerwać?" False
    if stop
    then return Nothing
    else _dateQuestion defDay defTime
  else return mDate
    where defDate = addUTCTime defTime defDay


date2str :: UTCTime -> String
date2str date = formatTime defaultTimeLocale "%F %R" date

getToday :: IO UTCTime
getToday = do
  date <- getCurrentTime
  return $ UTCTime (utctDay date) 0

_repeatQuestion :: IO (Maybe Repeat)
_repeatQuestion = do
  putStrLn "Podaj częstotliwość zadania"
  putStrLn "1:jednorazowe 2:co dzień 3:co tydzień 4:co miesiąc 5:co rok 0:anuluj"
  putStr "[1]|2|3|4|5|0: "
  hFlush stdout
  line <- getLine
  let ns = words line
  let n = if null ns
          then Just 1
          else readMaybe $ head ns
  if isNothing n
  then do
    stop <- _boolQuestion "Złe dane. Chcesz przerwać?" False
    if stop
       then return Nothing
       else _repeatQuestion
  else if n == Just 0
       then return Nothing
       else do
         let mR = join $ fmap _num2repeat n
         if isNothing mR
         then do
           stop1 <- _boolQuestion "Złe dane. Chcesz przerwać?" False
           if stop1
           then return Nothing
           else _repeatQuestion
         else return $ mR

_infoQuestion :: IO (Maybe String)
_infoQuestion = do
  putStrLn "Informacje dodatkowe: "
  infos <- _appendTillNNL 2 0 []
  print infos
  if null infos
  then return Nothing
  else return $ Just $ concat $ DL.intersperse " " $ reverse infos


_num2repeat :: Int -> Maybe Repeat
_num2repeat i
  | i == 1 = Just Once
  | i == 2 = Just Daily
  | i == 3 = Just Weekly
  | i == 4 = Just Monthly
  | i == 5 = Just Yearly
  | otherwise = Nothing
