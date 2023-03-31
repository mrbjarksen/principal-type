module Main where

import Data.Char (isSpace)

import System.Console.Haskeline

import Language.LambdaCalculus (Var(..), Term(..))
import Language.LambdaCalculus.Parser (parseTerm, ParserResult(..))
import Language.SimpleTypes.Inference (principalJudgement)

isExitStr :: String -> Bool
isExitStr = flip elem [":exit", ":quit", ":x", ":q"]

interrupt :: InputT IO a -> InputT IO a -> InputT IO a
interrupt f = handleInterrupt f . withInterrupt

repl :: String -> InputT IO ()
repl prev = do
  line <- interrupt (return $ Just "") $ getInputLine (if null prev then "λ> " else ".. ")
  case line of
    Nothing -> return ()
    Just "" -> repl prev
    Just s' -> if isExitStr s' then return () else do
      let s = prev ++ s'
      case parseTerm s of
        Error err -> outputStrLn err >> repl ""
        Incomplete -> repl (s ++ "\n")
        Success m -> do
          msg <- return $
            case principalJudgement m of
              Nothing -> show m ++ " is not typable"
              Just j  -> show j
          outputStrLn msg
          startRepl

startRepl :: InputT IO ()
startRepl = interrupt (outputStrLn "Interrupted" >> startRepl) $ repl ""

tabComplete :: Monad m => (String, String) -> m (String, [Completion])
tabComplete (l, r) = return (l, [Completion "\t" "" False])
  where padding m = replicate (m - (length l `mod` m)) ' '

main :: IO ()
main = runInputT (setComplete tabComplete defaultSettings) startRepl
