module Task
  ( Task(..)
  , new
  , doMe
  , isDone
  , isUndone
  , isDueDate
  , isDueDay
  , byTitle
  , byDeadline
  , TaskFilter
  , TaskSorter
  , shortFormat
  , longFormat
  )	where

{-
Strukutra danych Task - reprezentuje jedno zadanie
-}

import Data.Time (UTCTime, utctDay, Day)

import Repeat
import Utils

--Zadanie, zakładamy że są różne jeżeli różnią się co
--najmniej jednym polem
data Task = Task
  { _tskTitle :: String               -- Tytuł
  , _tskInfo :: Maybe String          -- Dodakowe informacje
  , _tskRepeat :: Repeat              -- Cykliczność
  , _tskDeadline :: UTCTime           -- Data wymaganej realizacji
  , _tskRealization :: Maybe UTCTime  -- Data kiedy zrealizwano zadanie
  } deriving (Eq, Ord, Show, Read)

--nowe, niewykonane zadanie
new :: String -> Maybe String -> Repeat -> UTCTime -> Task
new title minfo trepeat deadline =
  Task title minfo trepeat deadline Nothing

--wykonaj zadanie
--zwraca parę, (nowa forma zadania, ew nowe zadanie jeżeli cykliczne)
doMe :: Task -> UTCTime -> (Task, Maybe Task)
doMe task@Task{_tskRealization=Just _} _ = (task, Nothing)
doMe task time = (task{_tskRealization=Just time}, _nextRealization task)

--filtry zadań
type TaskFilter = Task -> Bool

isDone :: TaskFilter
isDone Task{_tskRealization = Nothing} = False
isDone _ = True

isUndone :: TaskFilter
isUndone task = not $ isDone task

isDueDate :: UTCTime -> TaskFilter
isDueDate date task = _tskDeadline task <= date

isDueDay :: Day -> TaskFilter
isDueDay day task = utctDay (_tskDeadline task) == day

--sortery zadań
type TaskSorter = Task -> Task -> Ordering

byTitle :: TaskSorter
byTitle t1 t2 = compare (_tskTitle t1) (_tskTitle t2)

byDeadline :: TaskSorter
byDeadline t1 t2 = compare (_tskDeadline t1) (_tskDeadline t2)

--konwersja Zadanie na string
shortFormat :: Task -> String
shortFormat task = 
  "\"" ++ title ++ "\" [" ++ rep ++ "] " ++ dline ++ " | " ++ real
    where
      title =  _tskTitle task
      rep = show $ _tskRepeat task
      dline = date2str $ _tskDeadline task
      real = maybe "--" date2str (_tskRealization task)

longFormat :: Task -> String
longFormat task = 
  "----------\n" ++
  "\"" ++ title ++ "\" [" ++ rep ++ "] " ++ dline ++ " | " ++ real ++ "\n" ++
  info ++
  "----------"
    where
      title =  _tskTitle task
      info = maybe "" (++ "\n") (_tskInfo task)
      rep = show $ _tskRepeat task
      dline = date2str $ _tskDeadline task
      real = maybe "--" date2str (_tskRealization task)


--stwórz nowe zadanie jeżeli bazowe jest cykliczne
_nextRealization :: Task -> Maybe Task
_nextRealization task =
  fmap nextTask (nextDate rep lastdline)
    where
      nextTask dline = task{_tskDeadline=dline}
      lastdline = _tskDeadline task
      rep = _tskRepeat task

