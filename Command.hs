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


data Command = Command {_cmdKey :: String
 , _cmdFormat :: String
 , _cmdInfo :: String
 , _cmdHandler :: CommandHandler
 }

type CommandHandler = [String] -> IO ()

cmdList :: [Command]
cmdList =
  [Command {_cmdKey="quit", _cmdFormat="[]",      _cmdInfo="quit program",    _cmdHandler=handleQuit}
  ,Command {_cmdKey="clear", _cmdFormat="[]",      _cmdInfo="clear terminal",  _cmdHandler=handleClear}
  ,Command {_cmdKey="new",  _cmdFormat="[title]", _cmdInfo="create new task", _cmdHandler=handleNew}
  ,Command {_cmdKey="list", _cmdFormat="[]",      _cmdInfo="list all tasks",  _cmdHandler=handleList}
  ]

cmdMap :: Map.Map String Command
cmdMap = Map.fromList $ map (\cmd -> (_cmdKey cmd, cmd)) cmdList


runCommand :: [String] -> IO()
runCommand [] = return ()
runCommand line@(kcmd:args) = case Map.lookup kcmd cmdMap of
  Nothing -> putStrLn $ "Zła komenda: " ++ unwords line
  Just cmd -> _cmdHandler cmd args

handleQuit :: CommandHandler
handleQuit _ = exitSuccess

handleClear :: CommandHandler
handleClear _ = clearScreen >> setCursorPosition 0 0


handleNew :: CommandHandler
handleNew xs = return ()

handleList ::  CommandHandler
handleList xs = return ()
