import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)
import System.Console.ANSI (Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , Underlining(..)
  , clearScreen
  , setCursorPosition
  , setSGRCode)

import System.Console.Haskeline (InputT
  , defaultSettings
  , getInputLine
  , runInputT
  , setComplete
  , outputStrLn)

import Logic
import Command

__PROMPT :: String
__PROMPT = "todo> "

main :: IO ()
main = do
  time <- getCurrentTime
  runInputT defaultSettings (interloop $ getEmptyStateData time)
--main = runInputT defaultSettings (interloop $ getEmptyStateData time)
  --where
    --time = return getCurrentTime

interloop :: TodoStateData -> InputT IO ()
interloop sd = do
  line <- getInputLine __PROMPT
  case line of
    Nothing -> return ()
    Just "time" -> lift () >> interloop sd
    Just cmdline -> liftIO (runCommand $ words cmdline) >> interloop sd
  where
    now = getCurrentTime
