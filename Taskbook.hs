module Taskbook
  ( Taskbook(..)
  , TaskNR
  , TaskSet
  , empty
  , insert
  , insertList
  , delete
  , deleteList
  , toList
  , doTask
  , doTaskList
  )	where

import Data.Maybe
import Data.Time (UTCTime)
import qualified Data.Set as DS

import Task

type TaskNR = Int
type TaskSet = DS.Set Task
newtype Taskbook = Taskbook {_tbTasks :: TaskSet}

empty :: Taskbook
empty = Taskbook DS.empty

insert :: Taskbook -> Task -> Taskbook
insert tbook task = tbook{_tbTasks=DS.insert task tasks}
  where
    tasks = _tbTasks tbook

insertList :: Taskbook -> [Task] -> Taskbook
insertList tbook [] = tbook
insertList tbook (x:xs) = insertList (insert tbook x) xs
  where
    tasks = _tbTasks tbook

delete :: Taskbook -> Task -> Taskbook
delete tbook task = tbook{_tbTasks=DS.delete task tasks}
  where
    tasks = _tbTasks tbook

deleteList :: Taskbook -> [Task] -> Taskbook
deleteList tbook [] = tbook
deleteList tbook (x:xs) = deleteList (delete tbook x) xs
  where
    tasks = _tbTasks tbook

member :: Taskbook -> Task -> Bool
member tbook task = DS.member task tasks
  where
    tasks = _tbTasks tbook

doTask :: Taskbook -> Task -> UTCTime -> Taskbook
doTask tbook task time =
  if not $ member tbook task
  then tbook
  else insertList (delete tbook task) (done:maybeToList mnext)
    where
      (done, mnext) = doMe task time

doTaskList :: Taskbook -> [Task] -> UTCTime -> Taskbook
doTaskList tbook [] _ = tbook
doTaskList tbook (x:xs) time = doTaskList (doTask tbook x time) xs time

toList :: Taskbook -> [Task]
toList = DS.toList . _tbTasks
--doneList :: Taskbook -> [Task]
--doneList = DS.toList . _tbTasks

--undoneList :: Taskbook -> [Task]
--undoneList = DS.toList . _tbTasks

--allList :: Taskbook -> [Task]
--allList tb = doneList tb ++ undoneList tb


--getNewTaskNR :: TaskSet -> TaskNR
--getNewTaskNR tmap
--  | DS.null tmap = 1
--  | otherwise = fst (DS.findMax tmap) + 1
