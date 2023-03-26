module Main where

import System.IO
import System.Console.Isocline
import Exp
import Parsing
import Printing
import REPLCommand
import Sugar
import Eval

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
          replLoop
        Eval exprStr -> do
          let complexExp = readComplexExp exprStr
              exp = simplifyComplexExp complexExp
              normalizedExp = normalize exp
              normalizedComplexExp = expandComplexExp normalizedExp
              formattedExpr = showComplexExp normalizedComplexExp
          liftIO $ putStrLn formattedExpr
          replLoop

parseREPLCommand :: String -> REPLCommand
parseREPLCommand input =
  case parse replCommand "" input of
    Left _ -> Eval input 
    Right cmd -> cmd
