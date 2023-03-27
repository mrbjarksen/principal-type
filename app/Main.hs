module Main where

import Data.Char (isSpace)

import System.Console.Haskeline

import Language.LambdaCalculus (Var(..), Term(..))
import Language.LambdaCalculus.Parser (parseTerm, ParserResult(..))
import Language.SimpleTypes.Inference (principalJudgement)

-- printType :: Term -> IO ()
-- printType m =
--   case principalJudgement m of
--        Nothing -> putStrLn $ show m ++ " is not typable"
--        Just j  -> print j

isExitStr :: String -> Bool
isExitStr = flip elem [":exit", ":quit", ":x", ":q"]

interrupt :: InputT IO a -> InputT IO a -> InputT IO a
interrupt f = handleInterrupt f . withInterrupt

-- getInputWith :: String -> InputT IO (Maybe String)
-- getInputWith "" = getInputLine "λ> "
-- getInputWith _  = getInputLine ".. "

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
        -- Success m -> do
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

-- main :: IO ()
-- main = do
--   let u = Abs (Var "u") (Abs (Var "x") (App (TermVar (Var "x")) (App (App (TermVar (Var "u")) (TermVar (Var "u"))) (TermVar (Var "x")))))
--   printType $ App (TermVar (Var "x")) (TermVar (Var "x"))
--   printType $ u
--   printType $ App u u
--   printType $ App (TermVar (Var "x")) (TermVar (Var "y"))
--   printType $ App (Abs (Var "x") (TermVar (Var "x"))) (Abs (Var "x") (TermVar (Var "y")))
--   printType $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (App (TermVar (Var "x")) (App (TermVar (Var "y")) (TermVar (Var "z"))))))
--   printType $ TermVar (Var "y")
--   printType $ Abs (Var "z") (TermVar (Var "y"))
--   printType $ Abs (Var "y") (Abs (Var "z") (TermVar (Var "y")))
--   printType $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (TermVar (Var "y"))))
--   printType $ Abs (Var "x") $ TermVar (Var "x")
--   printType $ Abs (Var "x") $ Abs (Var "y") $ TermVar (Var "x")
--   printType $ App (Abs (Var "x") $ Abs (Var "y") $ TermVar (Var "y")) (Abs (Var "z") $ TermVar (Var "z"))
--   printType $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (App (TermVar (Var "y")) (App (TermVar (Var "x")) (TermVar (Var "z"))))))
--   printType $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (App (App (TermVar (Var "x")) (TermVar (Var "z"))) (TermVar (Var "y")))))
--   printType $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (App (App (TermVar (Var "x")) (TermVar (Var "z"))) (App (TermVar (Var "y")) (TermVar (Var "z"))))))
--   printType $ Abs (Var "x") (Abs (Var "y") (App (App (TermVar (Var "x")) (TermVar (Var "y"))) (TermVar (Var "y"))))
--   printType $
--     App
--       (Abs (Var "v")
--         (Abs (Var "x")
--           (Abs (Var "y")
--             (Abs (Var "z")
--               (App
--                 (TermVar (Var "v"))
--                 (App
--                   (TermVar (Var "y"))
--                   (App
--                     (App
--                       (TermVar (Var "v"))
--                       (TermVar (Var "x"))
--                     )
--                     (TermVar (Var "z"))
--                   )
--                 )
--               )
--             )
--           )
--         )
--       )
--       (Abs (Var "x")
--         (TermVar (Var "x"))
--       )
--   printType $
--     Abs (Var "x")
--       (Abs (Var "y")
--         (Abs (Var "z")
--           (App
--             (Abs (Var "x")
--               (TermVar (Var "x"))
--             )
--             (App
--               (TermVar (Var "y"))
--               (App
--                 (App
--                   (Abs (Var "x")
--                     (TermVar (Var "x"))
--                   )
--                   (TermVar (Var "x"))
--                 )
--                 (TermVar (Var "z"))
--               )
--             )
--           )
--         )
--       )
--   printType $
--     App
--       (Abs (Var "x")
--         (TermVar (Var "x"))
--       )
--       (App
--         (TermVar (Var "y"))
--         (App
--           (App
--             (Abs (Var "x")
--               (TermVar (Var "x"))
--             )
--             (TermVar (Var "x"))
--           )
--           (TermVar (Var "z"))
--         )
--       )
--
