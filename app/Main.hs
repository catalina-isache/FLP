module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (unless)
import Text.ParserCombinators.Parsec (parse)

import Program (program, programEnv, normalizeEnv)
import Exp (Exp)
import Eval (eval)

-- Funcție pentru încărcarea fișierului prelude.mhs
loadPrelude :: IO (Maybe [Exp])
loadPrelude = do
  preludeContents <- readFile "prelude.mhs"
  case parse program "prelude.mhs" preludeContents of
    Left err -> do
      putStrLn ("Eroare la încărcarea fișierului prelude.mhs:\n" ++ show err)
      return Nothing
    Right definitions -> do
      let env = programEnv definitions
          normalizedDefs = map (\(k, v) -> (k, normalizeEnv env v)) (Map.toList env)
      return (Just normalizedDefs)

-- Funcție pentru evaluarea unei expresii în contextul dat de definițiile prelude-ului
evalWithPrelude :: Maybe [Exp] -> Exp -> IO ()
evalWithPrelude prelude exp =
  case prelude of
    Nothing -> return ()
    Just defs -> do
      let result = eval defs exp
      putStrLn ("Rezultat: " ++ show result)

main :: IO ()
main = do
  args <- getArgs
  prelude <- loadPrelude
  case args of
    [filename] -> do
      contents <- readFile filename
      case parse program filename contents of
        Left err -> putStrLn ("Eroare la parsarea programului:\n" ++ show err)
        Right definitions -> do
          let env = programEnv definitions
              normalizedDefs = map (\(k, v) -> (k, normalizeEnv env v)) (Map.toList env)
          unless (null definitions) $
            putStrLn "Definiții încărcate:"
          mapM_ (putStrLn . show) normalizedDefs
          putStrLn "Evaluare expresii:"
          mapM_ (evalWithPrelude prelude) (map (normalizeEnv env . definitionExp) definitions)
    _ -> putStrLn "Utilizare: interpreter <program.mhs>"
