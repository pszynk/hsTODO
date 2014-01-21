module Logic (TodoStateData
  , getEmptyStateData
)	where

import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime, addUTCTime)

import Task

data TodoStateData = TodoStateData {_tsdStartTime :: UTCTime, _tsdTimeStamp :: UTCTime, _tsdTasks :: TaskList}

type TodoState = State TodoStateData

getEmptyStateData :: UTCTime -> TodoStateData
getEmptyStateData time = TodoStateData time time getEmptyTaskList

setTime :: UTCTime -> UTCTime -> TodoState ()
setTime time timestamp= do
  tdata <- get
  let newtdata = tdata{_tsdStartTime=time, _tsdTimeStamp=timestamp}
  put newtdata
  return ()

getTime :: UTCTime -> TodoState UTCTime
getTime real = do
  tdata <- get
  let now = addUTCTime (diffUTCTime real (_tsdTimeStamp tdata)) (_tsdStartTime tdata)
  return now
