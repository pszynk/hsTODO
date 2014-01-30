module Taskbook
  ( Taskbook(..)
  , TaskNR
  , TaskSet
  , empty
  , insert
  )	where

import qualified Data.Set as DS

import Task

type TaskNR = Int
type TaskSet = DS.Set Task
data Taskbook = Taskbook
  { _tbDone :: TaskSet
  , _tbUndone :: TaskSet
  }

empty :: Taskbook
empty = Taskbook DS.empty DS.empty

insert :: Taskbook -> Task -> Taskbook
insert tbook task
  | isDone task = tbook{_tbDone=DS.insert task done}
  | otherwise = tbook{_tbUndone=DS.insert task undone}
  where
    done = _tbDone tbook
    undone= _tbUndone tbook

--getNewTaskNR :: TaskSet -> TaskNR
--getNewTaskNR tmap
--  | DS.null tmap = 1
--  | otherwise = fst (DS.findMax tmap) + 1
