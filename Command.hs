module Command(Command, CommandHandler, runCommand) where

{-
Implementacja komend interaktywnych programu
-}

import qualified Data.Map as Map
import Task

import System.Exit (exitSuccess)

import System.Console.ANSI (clearScreen
  , setCursorPosition)

import Control.Monad.Trans()
import Data.Time
--import Data.Time.Clock()
--import Data.Time.Format()
import System.Locale -- zastapic
import Control.Monad.State.Strict
import Control.Applicative
import qualified Data.Set as DS()

import qualified System.Console.Haskeline as HL()
import Data.Monoid (mappend)
import Data.Maybe
import System.IO

import Control.Exception
import Utils
import Repeat
import qualified Task()
import qualified Taskbook as TaskB
import Statedata
import qualified Text.Read as TR
import qualified Data.List as DL


--Komenda to rozkaz wydawany przez użytkowanika
--składa się z
-- -- klucza <- czyli stringa który wywołuje komendę
-- -- formatu (tylko pomocnicze)
-- -- informacji (tylko pomocnicze)
-- handlera <- czyli funckji odpowiedzalnej za wykonanie komendy
data Command = Command {_cmdKey :: String
 , _cmdFormat :: String
 , _cmdInfo :: String
 , _cmdHandler :: CommandHandler
 }

type SuccessMsg = Maybe String
type FailureMsg = String
-- komenda ma status sukces i możliwy komentzarz, lub porażka in
-- informacja o błędzie
type CommandStatus = Either FailureMsg SuccessMsg

-- funkcja wykonująca komędę, bierze listę poleceń użytkownika
-- w formie stringów i zwraca status komendy w kontekście stanu z IO
type CommandHandler = [String] -> StateWithIO CommandStatus

-- nazwy filtór zadań (jak podaje je użytkownik)
__DONE_FT :: String
__DONE_FT = "done"
__UNDONE_FT :: String
__UNDONE_FT = "undone"
__TODAY_FT :: String
__TODAY_FT = "today"
__OVERDUE_FT :: String
__OVERDUE_FT = "overdue"

-- nazwy sorterów zadań (jak podaje je użytkownik)
__DEADLINE_ST :: String
__DEADLINE_ST = "byDline"
__TITLE_ST :: String
__TITLE_ST = "byTitle"

-- wiadomość pomocnicza
__HELP_MSG :: String
__HELP_MSG =
  "todol help message\n"
  ++ "queries:\n"
  ++ "  " ++ __DONE_FT  ++ " -> done tasks\n"
  ++ "  " ++ __UNDONE_FT  ++ " -> undone tasks\n"
  ++ "  " ++ __TODAY_FT  ++ " -> tasks should be done today\n"
  ++ "  " ++ __OVERDUE_FT  ++ " -> overdue tasks\n"
  ++ "  " ++ __DEADLINE_ST  ++ " -> sort by deadline\n"
  ++ "  " ++ __TITLE_ST  ++ " -> sort by title\n"
  ++ "  Default query: all tasks by title\n"
  ++ "  Queries are joined with OR\n\n"
  ++ "cmd ... format ... cmd info\n"
  ++ "==========================="

-- wszystkie komendy programu
cmdList :: [Command]
cmdList =
  [Command {_cmdKey="quit",  _cmdFormat="[]", _cmdInfo="quit program",   _cmdHandler=handleQuit}
  ,Command {_cmdKey="clear", _cmdFormat="[]", _cmdInfo="clear terminal", _cmdHandler=handleClear}
  ,Command {_cmdKey="help",  _cmdFormat="[]", _cmdInfo="help message",   _cmdHandler=handleHelp}
  ,Command {_cmdKey="time", _cmdFormat="[]", _cmdInfo="change date",    _cmdHandler=handleTime}
  ,Command {_cmdKey="new",   _cmdFormat="<title>",    _cmdInfo="create new task",      _cmdHandler=handleNew}
  ,Command {_cmdKey="load",  _cmdFormat="<filename>", _cmdInfo="load tasks form file", _cmdHandler=handleLoad}
  ,Command {_cmdKey="del",   _cmdFormat="<query> | [<query>]", _cmdInfo="delete task", _cmdHandler=handleDelete}
  ,Command {_cmdKey="do",    _cmdFormat="<query> | [<query>]", _cmdInfo="mark task as done",  _cmdHandler=handleDo}
  ,Command {_cmdKey="ls",    _cmdFormat="<query> | [<query>]", _cmdInfo="list tasks",         _cmdHandler=handleList}
  ,Command {_cmdKey="save",  _cmdFormat="<query> | [<query>]", _cmdInfo="save tasks to file", _cmdHandler=handleSave}
  ]

-- mapa klucz komendy -> komenda
cmdMap :: Map.Map String Command
cmdMap = Map.fromList $ map (\cmd -> (_cmdKey cmd, cmd)) cmdList

-- wywołaj komendę z jej argumentami
runCommand :: CommandHandler
runCommand [] = return $ Left "nie podano komendy"
runCommand line@(kcmd:args) = case Map.lookup kcmd cmdMap of
  Nothing -> return $ Left $ "Zła komenda: " ++ unwords line
  Just cmd -> _cmdHandler cmd args


-- KOMENDY --------------------------------------------------

handleQuit :: CommandHandler
handleQuit _ = do
  b <- liftIO $ _boolQuestion "Zamknąć program?" False
  when b $ liftIO exitSuccess
  return $ Right Nothing

handleClear :: CommandHandler
handleClear _ = do
  liftIO _clearScreen
  return $ Right Nothing

handleHelp :: CommandHandler
handleHelp _ = do
  liftIO $ putStrLn __HELP_MSG
  liftIO $ mapM_ (putStrLn . _cmd2str) cmdList
  return $ Right Nothing

handleDelete :: CommandHandler
handleDelete query = do
  (tlist, _) <- _queryState query
  liftIO $ _printShortTasks tlist
  mchose <- liftIO $ _indexQuestion "Wybierz indeks do usunięcia" (length tlist)
  if isNothing mchose
  then return $ Left "Przerwano usuwanie"
  else do
    let Just chose  = mchose
    let del = chose tlist
    removeTaskList del
    return $ Right Nothing

handleNew :: CommandHandler
handleNew [] = return $ Left "Podaj tytuł nowego zadania"
handleNew xs = do
  liftIO $ putStrLn  "Tworzenie nowego zadania"
  let title = unwords xs
  liftIO $ putStrLn $ "Tytuł: '" ++ title ++ "'"
  minfo <- liftIO _infoQuestion
  today <- getToday
  mdate <- liftIO $ _dateQuestion "Podaj datę zadania" today __TIME_24H
  if isNothing mdate
  then return $ Left "Przerwano tworzenie zadania"
  else do
    let Just date = mdate
    liftIO $ putStrLn $ "Data zadania: " ++ date2str date
    mrepeat <- liftIO _repeatQuestion
    if isNothing mrepeat
    then return $ Left "Przerwano tworzenie zadania"
    else do
      let Just rrepeat = mrepeat
      let task = Task.new title minfo rrepeat date
      liftIO $ putStrLn "Tworzenie zadania:"
      liftIO $ putStrLn $ longFormat task
      ifadd <- liftIO $ _boolQuestion "Potwierdz nowe zadanie" True
      if ifadd
      then do
        addTask task
        return $ Right $ Just "Dodano nowe zadanie"
      else return $ Left "Odrzucono nowe zadanie"

handleList :: CommandHandler
handleList xs = do
  (tlist, _) <- _queryState xs
  liftIO $ _printLongTasks tlist
  return $ Right Nothing

handleTime ::  CommandHandler
handleTime _ = do
  today <- getToday
  time <- getTimeNow
  mdate <- liftIO $ _dateQuestion "Podaj nowy czas" today time
  if isNothing mdate
  then return $ Left "Nie zmieniono daty"
  else do
    let Just date = mdate
    setTime date
    return $ Right $ Just "Zmieniono datę"

handleDo :: CommandHandler
handleDo query = do
  (tlist', _) <- _queryState query
  let tlist = filter isUndone tlist'
  liftIO $ _printShortTasks tlist
  mchose <- liftIO $ _indexQuestion "Wybierz indeks zadania wykonanego" (length tlist)
  if isNothing mchose
  then return $ Left "Przerwano wykonywanie"
  else do
    let Just chose  = mchose
    let done = chose tlist
    liftIO $ putStrLn $ "Wykonano: " ++ show done
    setDoTaskList done
    return $ Right Nothing

handleSave :: CommandHandler
handleSave query = do
  (tlist, _) <- _queryState query
  liftIO $ putStr "Podaj ścieżkę do pliku: "
  liftIO $ hFlush stdout
  pps <- liftIO $ words <$> getLine
  if null pps
  then return $ Left "Przerwano zapis do pliku"
  else liftIO $ _writeTasksToFile (head pps) tlist


handleLoad :: CommandHandler
handleLoad [] = return $ Left "Nie podano ścieżki do pliku"
handleLoad (filepath:_)= do
  (rtasks, cmdsts) <- liftIO $ _readTasksFromFile filepath
  case cmdsts of
    (Left _) -> return cmdsts
    (Right _) -> do
      liftIO $ _printShortTasks rtasks
      liftIO $ putStr "Podaj <query> | [<query>] do wczytania zadań [all]: "
      liftIO $ hFlush stdout
      query <- liftIO $ words <$> getLine
      (tlist, _) <- _queryCustomList query rtasks
      addTaskList tlist
      return cmdsts


-- FUNKCJE FORMATUJĄCE NA STRINGiI -------------------------------

_cmd2str :: Command -> String
_cmd2str cmd = _cmdKey cmd ++ " ... " ++ _cmdFormat cmd ++ " ... " ++ _cmdInfo cmd

-- zadanie w któtkiej formie
_printShortTasks :: [Task] -> IO ()
_printShortTasks tasks = mapM_ putStrLn (_toEnumerate shortFormat tasks show 1 ") ")

-- zadanie w długie formie
_printLongTasks :: [Task] -> IO ()
_printLongTasks tasks = mapM_ putStrLn (_toEnumerate longFormat tasks show 1 ") ")


-- FUNKCJE INTERAKCJI Z UŻYTKOWNIKIEM -------------------------------

-- Proste pytanie Tak/Nie z możliwością ustawienia odpowiedzi domyślnej
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


-- spytaj się użytkownika o datę
_dateQuestion :: String -> UTCTime -> NominalDiffTime -> IO (Maybe UTCTime)
_dateQuestion msg defDay defTime = do
  putStr $ msg ++ " (format YYYY-MM-DD, lub HH-MM lub oba na raz) [" ++ date2str defDate ++ "]: "
  hFlush stdout
  line <- getLine
  let mDate = _parseDate (words line) defDay defTime
  if isNothing mDate
  then do
    putStrLn "Błędna data"
    stop <- _boolQuestion "Chcesz przerwać?" False
    if stop
    then return Nothing
    else _dateQuestion msg defDay defTime
  else return mDate
    where defDate = addUTCTime defTime defDay

-- spytaj się użytkwnika o cykliczność
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
          else TR.readMaybe $ head ns
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
         else return mR

-- spytaj się użytkownia o dodatkowe informacje
_infoQuestion :: IO (Maybe String)
_infoQuestion = do
  putStrLn "Informacje dodatkowe: "
  infos <- _appendTillNNL 2 0 []
  return (if null infos
         then Nothing
         else Just $ unwords $ reverse infos)

-- spytaj użytkownika o indeks z listy
_indexQuestion :: String -> Int -> IO (Maybe ([a] -> [a]))
_indexQuestion msg maxIdx = do
  putStr $ msg ++ " (`*' <- wszystkie): "
  hFlush stdout
  widxs <- words <$> getLine
  if null widxs
  then do
    stop <- _boolQuestion "Chcesz przerwać?" False
    if stop
    then return Nothing
    else _indexQuestion msg maxIdx
  else do
    let widx = head widxs
    if widx == "*"
    then return $ Just id
    else do
      let midx = TR.readMaybe widx :: Maybe Int
      if isNothing midx
      then do
        stop <- _boolQuestion "Źle zapisany indeks, chcesz przerwać?" False
        if stop
        then return Nothing
        else _indexQuestion msg maxIdx
      else do
        let Just idx = midx
        if idx < 0 || idx > maxIdx
        then do
          stop <- _boolQuestion "Zły zakres indeku, chcesz przerwać?" False
          if stop
          then return Nothing
          else _indexQuestion msg maxIdx
        else return $ Just (\list -> [list !! (idx - 1)])


-- FUNKCJE PARSUJĄCE STRINGI ---------------------------------

-- zapytania list do obecnego stanu programu
_queryState :: [String] -> StateWithIO ([Task], [String])
_queryState query = do
  (fquery, unknown) <- _parseQuery query
  tbook <- getTaskbook
  return (fquery $ TaskB.toList tbook, unknown)

-- zapytania list do podanej listy zadań
_queryCustomList :: [String] -> [Task] -> StateWithIO ([Task], [String])
_queryCustomList _ [] = return ([], [])
_queryCustomList query tlist = do
  (fquery, unknown) <- _parseQuery query
  return (fquery tlist, unknown)

-- parsuj wybór wyszukiwania i do tworzenia listy
_parseQuery :: [String] -> StateWithIO ([Task] -> [Task], [String])
_parseQuery query = do
  (tf, notFQuery) <- _parseTaskFilters query
  (sf, restQuery) <- _parseTaskSorters notFQuery
  queryMsg restQuery
  return (DL.sortBy sf . filter tf, restQuery)
    where
      queryMsg [] = return ()
      queryMsg xs =
        liftIO $ putStrLn $ "Ignoruję polecenia: " ++ foldr (\q acc -> q ++ ", " ++ acc) "" xs

-- parsuj wybrane filtry zadań
_parseTaskFilters :: [String] -> StateWithIO (TaskFilter, [String])
_parseTaskFilters [] = return (const True, [])
_parseTaskFilters list = ptf (DL.nub list)
  where
    ptf :: [String] -> StateWithIO (TaskFilter, [String])
    ptf [] = return (const False, [])
    ptf (x:xs) = do
      (tf, unknown) <- ptf xs
      mfil <- _str2filter x
      if isNothing mfil
      then return (tf, x:unknown)
      else let Just f = mfil in return (\t -> f t || tf t, unknown)


-- parsuj wybrane sortery zadań
_parseTaskSorters :: [String] -> StateWithIO (TaskSorter, [String])
_parseTaskSorters list = pts (DL.nub list)
  where
    pts :: [String] -> StateWithIO (TaskSorter, [String])
    pts [] = return (\_ _ -> EQ, [])
    pts (x:xs) = do
      (ts, unknown) <- pts xs
      msor <- _str2sorter x
      if isNothing msor
      then return (ts, x:unknown)
      else let Just s = msor
               in return (\t1 t2 -> s t1 t2 `mappend` ts t1 t2, unknown)

-- parsuj dzień
_parseDay :: String -> Maybe UTCTime
_parseDay = parseTime defaultTimeLocale "%F"

-- parsuj czas
_parseTime :: String -> Maybe NominalDiffTime
_parseTime time = pure diffUTCTime <*> parseTime defaultTimeLocale "%R" time <*> Just  __EPOCH_DAY

-- parsuj datę
-- przy podanym domyślnym dniu i czasie,
-- jeżeli użytkownik nie poda dnia, czasu lub oby tych wartości
-- to brane są wartości domyślne
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


-- FUNKCJE MAPUJĄCE STRINGI ---------------------------------

--mapuj numer na cykliczność
_num2repeat :: Int -> Maybe Repeat
_num2repeat i
  | i == 1 = Just Once
  | i == 2 = Just Daily
  | i == 3 = Just Weekly
  | i == 4 = Just Monthly
  | i == 5 = Just Yearly
  | otherwise = Nothing
-- mapuj polecenia na filtry wyszukiwań
_str2filter :: String -> StateWithIO (Maybe TaskFilter)
_str2filter str
  | str == __DONE_FT = return $ Just isDone
  | str == __UNDONE_FT = return $ Just isUndone
  | str == __TODAY_FT = do
    time <- getTestTime
    return $ Just $ isDueDay $ utctDay time
  | str == __OVERDUE_FT = do
    time <- getTestTime
    return $ Just $ isDueDate time
  | otherwise = return Nothing

-- mapuj polecenia na sortery
_str2sorter :: String -> StateWithIO (Maybe TaskSorter)
_str2sorter str
  | str == __TITLE_ST = return $ Just byTitle
  | str == __DEADLINE_ST = return $ Just byDeadline
  | otherwise = return Nothing

-- IO Z PLIKÓW ------------------------------------------------

-- wyczytaj zadania z podanego pliku
_readTasksFromFile :: FilePath -> IO ([Task], CommandStatus)
_readTasksFromFile filepath =
  handle (\e -> do
    msg <- _IOExceptionHandler "Błąd czytania z pliku: " e
    return ([], msg)) $ do
    content <- readFile filepath
    let tasks = mapMaybe TR.readMaybe (lines content) :: [Task]
    return (tasks, Right $ Just $ "Odczytano " ++ show (length tasks) ++ " z pliku: " ++ filepath)

-- zapisz zadania do podanego pliku
_writeTasksToFile :: FilePath -> [Task] -> IO CommandStatus
_writeTasksToFile _ [] = return $ Left "Nie ma zadań do zapisania"
_writeTasksToFile filepath tasks =
  handle (_IOExceptionHandler "Błąd pisania do pliku: ") $ do
    withFile filepath WriteMode (\h -> mapM (hPrint h) tasks)
    return $ Right $ Just $ "Sukces, zapisano dane do pliku: " ++ filepath

-- obsługa wyjątku IO
_IOExceptionHandler :: String -> IOException -> IO CommandStatus
_IOExceptionHandler msg e = return $ Left $ msg ++ show e

-- FUNKCJE POMOCNICZE -------------------------------------------

-- zwraca listę obiektów w postaci listy stringów z numerami na początku
_toEnumerate :: (a -> String) -> [a] -> (Int -> String) -> Int -> String -> [String]
_toEnumerate _ [] _ _ _= []
_toEnumerate xprt (x:xs) iprt n deli = str:_toEnumerate xprt xs iprt (n+1) deli
  where
    str = iprt n ++ deli ++ xprt x

-- czyść ekran terminala
_clearScreen :: IO ()
_clearScreen = do
  clearScreen
  setCursorPosition 0 0

-- zwróć liste stringów będących kolejnymi liniami
-- wpisywanymi przez użytkownia, aż użytkownik
-- nie wciśnie Enter odpowiednią ilość razy
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

