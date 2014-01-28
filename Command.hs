module Command(Command, CommandHandler, runCommand) where


import qualified Data.Map as Map

import System.Exit (exitSuccess)

import System.Console.ANSI (Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , Underlining(..)
  , clearScreen
  , setCursorPosition
  , setSGRCode)

import Control.Monad.Trans (liftIO)
import Data.Time
import Data.Time.Format
import System.Locale -- zastapic
import Control.Monad.State.Strict

import System.Console.Haskeline (InputT
  , defaultSettings
  , getInputLine
  , runInputT
  , setComplete
  , outputStrLn)

import Control.Exception
import Logic

data Command = Command {_cmdKey :: String
 , _cmdFormat :: String
 , _cmdInfo :: String
 , _cmdHandler :: CommandHandler
 }

type SuccessMsg = Maybe String
type FailureMsg = String
type CommandStatus = Either FailureMsg SuccessMsg

type CommandHandler = [String] -> InputT TodoStateTIO CommandStatus

cmdList :: [Command]
cmdList =
  [Command {_cmdKey="quit", _cmdFormat="[]",      _cmdInfo="quit program",    _cmdHandler=handleQuit}
  ,Command {_cmdKey="clear", _cmdFormat="[]",      _cmdInfo="clear terminal",  _cmdHandler=handleClear}
  ,Command {_cmdKey="new",  _cmdFormat="[title]", _cmdInfo="create new task", _cmdHandler=handleNew}
  ,Command {_cmdKey="list", _cmdFormat="[]",      _cmdInfo="list all tasks",  _cmdHandler=handleList}
  ,Command {_cmdKey="cdate", _cmdFormat="[]",      _cmdInfo="change date",  _cmdHandler=handleCdate}
  ,Command {_cmdKey="ctime", _cmdFormat="[]",      _cmdInfo="change time",  _cmdHandler=handleList}
  ]

cmdMap :: Map.Map String Command
cmdMap = Map.fromList $ map (\cmd -> (_cmdKey cmd, cmd)) cmdList

runCommand :: CommandHandler
runCommand [] = return $ Left "nie podano komendy"
runCommand line@(kcmd:args) = case Map.lookup kcmd cmdMap of
  Nothing -> return $ Left $ "Zła komenda: " ++ unwords line-- liftIO $ putStrLn $ "Zła komenda: " ++ unwords line
  Just cmd -> _cmdHandler cmd args

handleQuit :: CommandHandler
handleQuit _ = liftIO exitSuccess


handleClear :: CommandHandler
handleClear _ = do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  return $ Right Nothing

handleNew :: CommandHandler
handleNew xs = return $ Right Nothing

handleList ::  CommandHandler
handleList xs = return $ Right Nothing

handleCdate ::  CommandHandler
handleCdate (datestr:_) = do
  let mdate = parseTime defaultTimeLocale "%F" datestr
  case mdate of
    Nothing -> return $ Left $ "Zły format daty: " ++ datestr
    Just date -> do
      lift $ setTime date
      return $ Right $ Just "Zmieniono datę"
