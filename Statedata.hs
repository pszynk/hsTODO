module Statedata where

{-
Program wykorzystuje transformer monad StateT
Stan programu trzymany jest w typie StateWithIO
co pozwala na jednoczesne korzystanie z właściwości
monad State i IO
-}

import Control.Monad.State.Strict
import Control.Monad.Trans()
import Data.Time.Clock


import Task
import Repeat()
import qualified Taskbook as TB

-- Stan programu: 
-- -- Czas 0, 
-- -- Czas w którym ustalono czas 0
-- -- Książka zadań
-- Czas 0 to czas od jakiego będziemy liczyć upływ czasu (t0)
-- Timestamp to czas rzeczywisty w jakim dokonano ostatnie zmiany czasu 0 (ts)
-- Czas symulowany wyliczany jest ze wzoru t0 + (tr - ts)
-- gdzie tr to czas rzeczywisty
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

-- jaki mamy dziś symulowany dzień
getToday ::  StateWithIO UTCTime
getToday = do
  date <- getTestTime
  return $ UTCTime (utctDay date) 0

-- jaki mamy symulowany czas
getTimeNow ::  StateWithIO NominalDiffTime
getTimeNow = do
  date <- getTestTime
  return $ diffUTCTime date $ UTCTime (utctDay date) 0

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
