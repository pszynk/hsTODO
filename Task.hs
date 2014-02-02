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


import Data.Time (UTCTime, utctDay, Day)

import Repeat
import Utils

--data TaskRealization = TaskRealization {_trDeadline :: UTCTime, _trDoneDate :: UTCTime} deriving (Show)

 --_tskID :: TaskNR
  --, _tskPastRealizations :: [TaskRealization]
data Task = Task
  { _tskTitle :: String
  , _tskInfo :: Maybe String
  , _tskRepeat :: Repeat
  , _tskDeadline :: UTCTime
  , _tskRealization :: Maybe UTCTime
  } deriving (Eq, Ord, Show, Read)

new :: String -> Maybe String -> Repeat -> UTCTime -> Task
new title minfo trepeat deadline =
  Task title minfo trepeat deadline Nothing


type TaskFilter = Task -> Bool

isDone :: TaskFilter
isDone Task{_tskRealization = Nothing} = False
isDone _ = True

isUndone :: TaskFilter
isUndone task = not $ isDone task

isDueDate :: UTCTime -> TaskFilter
isDueDate date task = _tskDeadline task <= date

isDueDay :: Day -> TaskFilter
isDueDay day task = utctDay (_tskDeadline task) <= day

type TaskSorter = Task -> Task -> Ordering

byTitle :: TaskSorter
byTitle t1 t2 = compare (_tskTitle t1) (_tskTitle t2)

byDeadline :: TaskSorter
byDeadline t1 t2 = compare (_tskDeadline t1) (_tskDeadline t2)

doMe :: Task -> UTCTime -> (Task, Maybe Task)
doMe task@Task{_tskRealization=Just _} _ = (task, Nothing)
doMe task time = (task{_tskRealization=Just time}, _nextRealization task)

_nextRealization :: Task -> Maybe Task
_nextRealization task =
  fmap nextTask (nextDate rep lastdline)
    where
      nextTask dline = task{_tskDeadline=dline}
      lastdline = _tskDeadline task
      rep = _tskRepeat task

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
  "\"" ++ title ++ "\" [" ++ rep ++ "] " ++ dline ++ " | " ++ real ++
  info ++
  "----------\n"
    where
      title =  _tskTitle task
      info = maybe "" (++ "\n") (_tskInfo task)
      rep = show $ _tskRepeat task
      dline = date2str $ _tskDeadline task
      real = maybe "--" date2str (_tskRealization task)
