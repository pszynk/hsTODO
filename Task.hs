module Task (TaskList
  , getEmptyTaskList
)	where

import Data.Time.Clock (UTCTime, NominalDiffTime, utctDay)
data Repeatability = Once | Daily | Weekly | Monthly | Yearly |
  CustomPeriod {_repPeriod :: NominalDiffTime} | CustomOccurrence {_repOccurrence :: [UTCTime]}

data TaskRealization = TaskRealization {_trDeadline :: UTCTime, _trDoneDate :: UTCTime}

type TaskID = Int

data Task = Task {_tskID :: TaskID
  , _tskTitle :: String
  , _tskInfo :: Maybe String
  , _tskRepeat :: Repeatability
  , _tskPastRealizations :: [TaskRealization]
  , _tskNextDeadline :: Maybe UTCTime
  }

type TaskList = [Task]

getEmptyTaskList :: [Task]
getEmptyTaskList = []


nextDate :: Repeatability -> UTCTime -> Maybe UTCTime
nextDate Once _ = Nothing
nextDate _ date = Just date --TODO poprawić dodawanie dat

addNewTask :: TaskList -> Task -> TaskList
addNewTask tlist task = task:tlist

createNewTask :: TaskID -> String -> Maybe String -> Repeatability -> UTCTime -> Task
createNewTask tid title minfo trepeat deadline =
  Task tid title minfo trepeat [] (Just deadline)

type TaskFilter = Task -> Bool

doneTasksFilter :: TaskFilter
doneTasksFilter task = not $ null $ _tskPastRealizations task

overdueTasksFilter :: UTCTime -> TaskFilter
overdueTasksFilter _ Task {_tskNextDeadline=Nothing} = False
overdueTasksFilter time Task {_tskNextDeadline=Just deadline} = time < deadline

todayTasksFilter :: UTCTime -> TaskFilter
todayTasksFilter _ Task {_tskNextDeadline=Nothing} = False
todayTasksFilter time Task {_tskNextDeadline=Just deadline} = utctDay time == utctDay deadline

type TaskError = String

doTask :: Task -> UTCTime -> Either TaskError Task
doTask Task {_tskNextDeadline=Nothing} _ = Left "O"
doTask task time = Right task{_tskPastRealizations = TaskRealization deadline time : past
    , _tskNextDeadline=nextDate trepeat deadline}
      where
        trepeat = _tskRepeat task
        past = _tskPastRealizations task
        Just deadline = _tskNextDeadline task

--setupTask task
--addTask tlist task
