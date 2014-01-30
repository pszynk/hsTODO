--module Logic (StateWithIO
  --, Statedata
  --, getEmptyStatedata
  --, runStateT
  --, lift
  --, get
  --, getTime
--)	where
module Statedata where

import Control.Monad.State.Strict
import Control.Monad.Trans()
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime)


import Task
import Repeat()
import qualified Taskbook as TB

data Statedata = Statedata {_sdStartTime :: UTCTime, _sdTimeStamp :: UTCTime, _sdTasks :: TB.Taskbook}

type StateWithIO a = StateT Statedata IO a

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
  let tasks = _sdTasks tdata
  let newtdata = tdata{_sdTasks=TB.insert tasks task}
  put newtdata
  return ()

getTime :: StateWithIO UTCTime
getTime = do
  tdata <- get
  real <- liftIO getCurrentTime
  let now = addUTCTime (diffUTCTime real (_sdTimeStamp tdata)) (_sdStartTime tdata)
  return now
