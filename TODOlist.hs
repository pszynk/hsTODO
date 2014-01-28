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
import Logic
import Command

__PROMPT :: String
__PROMPT = "todo> "

main :: IO ()
main = do
  time <- getCurrentTime
  (runStateT (runInputT defaultSettings interloop) $ getEmptyStateData time) >> return ()

interloop :: InputT TodoStateTIO ()
interloop = do
  time <- lift getTime
  line <- getInputLine $ (show time) ++ __PROMPT
  case line of
    Nothing -> return ()
    Just cmdline -> do
      status <- runCommand $ words cmdline
      case status of
        (Left err) -> outputStrLn err
        (Right Nothing) -> return ()
        (Right (Just scc)) -> outputStrLn scc
      interloop

