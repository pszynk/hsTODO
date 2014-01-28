--module Logic (TodoStateTIO
  --, TodoStateData
  --, getEmptyStateData
  --, runStateT
  --, lift
  --, get
  --, getTime
--)	where
module Logic where

import Control.Monad.State.Strict
import Control.Monad.Trans (liftIO)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime)

import Task

data TodoStateData = TodoStateData {_tsdStartTime :: UTCTime, _tsdTimeStamp :: UTCTime, _tsdTasks :: TaskList}

type TodoStateTIO = StateT TodoStateData IO

getEmptyStateData :: UTCTime -> TodoStateData
getEmptyStateData time = TodoStateData time time getEmptyTaskList

setTime :: UTCTime -> TodoStateTIO ()
setTime time = do
  tdata <- get
  timestamp <- liftIO getCurrentTime
  let newtdata = tdata{_tsdStartTime=time, _tsdTimeStamp=timestamp}
  put newtdata
  return ()


getTime :: TodoStateTIO UTCTime
getTime = do
  tdata <- get
  real <- liftIO getCurrentTime
  let now = addUTCTime (diffUTCTime real (_tsdTimeStamp tdata)) (_tsdStartTime tdata)
  return now
