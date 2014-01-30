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

import Control.Monad.State.Strict
import Statedata
import Command
import Utils

__PROMPT :: String
__PROMPT = "todo" ++ __PROMPT_SIGN ++ " "

main :: IO ()
main = do
  time <- getCurrentTime
  void (runStateT (runInputT defaultSettings interloop) $ getEmptyStateData time)

interloop :: InputT TodoStateTIO ()
interloop = do
  time <- lift getTime
  line <- getInputLine $ show time ++ __PROMPT
  case line of
    Nothing -> return ()
    Just cmdline -> do
      status <- lift $ runCommand $ words cmdline
      case status of
        (Left err) -> outputStrLn err
        (Right Nothing) -> return ()
        (Right (Just scc)) -> outputStrLn scc
      interloop

