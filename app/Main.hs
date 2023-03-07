module Main where

import System.IO
import System.Console.Isocline
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runInputT defaultSettings replLoop

replLoop :: InputT IO ()
replLoop = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> return ()
    Just input -> do
      let command = parseREPLCommand input
      case command of
        Quit -> return ()
        Load filePath -> do
          liftIO $ putStrLn $ "Loading file: " ++ filePath
          -- Perform file loading logic here
          replLoop
        Eval exprStr -> do
          let parsedExpr = parseExpr exprStr
          case parsedExpr of
            Left err -> liftIO $ putStrLn $ "Parse error: " ++ show err
            Right expr -> do
              let formattedExpr = showExp expr
              liftIO $ putStrLn formattedExpr
          replLoop

parseREPLCommand :: String -> REPLCommand
parseREPLCommand input =
  case parse replCommand "" input of
    Left _ -> Eval input -- Treat input as Eval command if parsing fails
    Right cmd -> cmd
