module Statedata where

import Control.Monad.State.Strict
import Control.Monad.Trans()
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime)


import Task
import Repeat()
import qualified Taskbook as TB

data Statedata = Statedata {_sdStartTime :: UTCTime, _sdTimeStamp :: UTCTime, _sdTasks :: TB.Taskbook}

type StateWithIO = StateT Statedata IO

empty :: UTCTime -> Statedata
empty time = Statedata time time TB.empty

setTime :: UTCTime -> StateWithIO ()
setTime time = do
  tdata <- get
  timestamp <- liftIO getCurrentTime
  let newtdata = tdata{_sdStartTime=time, _sdTimeStamp=timestamp}
  put newtdata
  return ()

--getNextTaskNR :: StateWithIO TaskNR
--getNextTaskNR = do
--  s <- get
--  return $ getNewTaskNR $ _tsdTasks s

--createTask :: String -> Maybe String -> Repeat-> UTCTime -> StateWithIO Task
--createTask title minfo trepeat deadline = do
--  tid <- getNextTaskNR
--  return $ createNewTask tid title minfo trepeat deadline

addTask :: Task -> StateWithIO ()
addTask task = do
  tdata <- get
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.insert tbook task}
  put newtdata
  return ()

addTaskList :: [Task] -> StateWithIO ()
addTaskList [] = return ()
addTaskList tasks = do
  tdata <- get
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.insertList tbook tasks}
  put newtdata
  return ()

removeTask :: Task -> StateWithIO ()
removeTask task = do
  tdata <- get
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.delete tbook task}
  put newtdata
  return ()

removeTaskList :: [Task] -> StateWithIO ()
removeTaskList [] = return ()
removeTaskList tasks = do
  tdata <- get
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.deleteList tbook tasks}
  put newtdata
  return ()

getTestTime :: StateWithIO UTCTime
getTestTime = do
  tdata <- get
  real <- liftIO getCurrentTime
  let now = addUTCTime (diffUTCTime real (_sdTimeStamp tdata)) (_sdStartTime tdata)
  return now

getTaskbook :: StateWithIO TB.Taskbook
getTaskbook = do
  tdata <- get
  return $ _sdTasks tdata

setDoTask :: Task -> StateWithIO ()
setDoTask task = do
  tdata <- get
  time <- getTestTime
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.doTask tbook task time}
  put newtdata
  return ()

setDoTaskList :: [Task] -> StateWithIO ()
setDoTaskList [] = return ()
setDoTaskList task = do
  tdata <- get
  time <- getTestTime
  let tbook = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.doTaskList tbook task time}
  put newtdata
  return ()

--setTaskbook :: TB.Taskbook -> StateWithIO ()
--setTaskbook tbook = do
  --tdata <- get
  --let newtdata = tdata{_sdTasks=tbook}
  --put newtdata
  --return ()
