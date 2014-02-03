{-
Plik z funkcją main programu.
Program wykorzsytuje moduł haskeline
dla zwiększenia wykody użytkownika
-}
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans()

import System.Console.Haskeline (InputT
  , defaultSettings
  , getInputLine
  , runInputT
  , outputStrLn)

import Control.Monad.State.Strict
import Statedata as SD
import Command
import Utils

__PROMPT :: String
__PROMPT = " todo" ++ __PROMPT_SIGN ++ " "

-- funkcja main tworzy pusty stan i wywołuje interaktywną pętlę
main :: IO ()
main = do
  time <- getCurrentTime
  void (runStateT (runInputT defaultSettings interloop) $ SD.empty time)

-- inteaktywna pętla, zczytuje polecenia użytkownia, wywołuje odpowiednie
-- komendy
interloop :: InputT StateWithIO ()
interloop = do
  time <- lift getTestTime
  line <- getInputLine $ date2str time ++ __PROMPT
  case line of
    Nothing -> return ()
    Just cmdline -> do
      status <- lift $ runCommand $ words cmdline
      case status of
        (Left err) -> outputStrLn err
        (Right Nothing) -> return ()
        (Right (Just scc)) -> outputStrLn scc
      interloop

