module Task 
  ( Task(..)
  , new
  , isDone
  , isUndone
  )	where


import Data.Time (UTCTime)

import Repeat

--data TaskRealization = TaskRealization {_trDeadline :: UTCTime, _trDoneDate :: UTCTime} deriving (Show)

 --_tskID :: TaskNR
  --, _tskPastRealizations :: [TaskRealization]
data Task = Task
  { _tskTitle :: String
  , _tskInfo :: Maybe String
  , _tskRepeat :: Repeat
  , _tskDeadline :: UTCTime
  , _tskRealization :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

new :: String -> Maybe String -> Repeat -> UTCTime -> Task
new title minfo trepeat deadline =
  Task title minfo trepeat deadline Nothing

isDone :: Task -> Bool
isDone Task{_tskRealization = Nothing} = True
isDone _ = False

isUndone :: Task -> Bool
isUndone task = not $ isDone task


type TaskFilter = Task -> Bool

--doneTasksFilter :: TaskFilter
--doneTasksFilter task = not $ null $ _tskPastRealizations task

--overdueTasksFilter :: UTCTime -> TaskFilter
--overdueTasksFilter _ Task {_tskDeadline=Nothing} = False
--overdueTasksFilter time Task {_tskDeadline=Just deadline} = time < deadline

--todayTasksFilter :: UTCTime -> TaskFilter
--todayTasksFilter _ Task {_tskDeadline=Nothing} = False
--todayTasksFilter time Task {_tskDeadline=Just deadline} = utctDay time == utctDay deadline


--type TaskError = String

--doTask :: Task -> UTCTime -> Either TaskError Task
--doTask Task {_tskDeadline=Nothing} _ = Left "Zadanie zostało już wykonane"
--doTask task time = Right task{_tskPastRealizations = TaskRealization deadline time : past
--    , _tskDeadline=nextDate trepeat deadline}
--      where
--        trepeat = _tskRepeat task
--        past = _tskPastRealizations task
--        Just deadline = _tskDeadline task

--setupTask task
--addTask tlist task
