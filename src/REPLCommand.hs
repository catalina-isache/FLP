module REPLCommand where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec (anyChar)
import Control.Applicative (many)

data REPLCommand
  = Quit
  | Load String
  | Eval String


replDef :: LanguageDef st
replDef = emptyDef

replTokenParser :: Token.TokenParser st
replTokenParser = Token.makeTokenParser replDef

quitCommand :: Parser REPLCommand
quitCommand = Quit <$ (Token.symbol replTokenParser ":q" <|> Token.symbol replTokenParser ":quit")

loadCommand :: Parser REPLCommand
loadCommand = Load <$> (Token.symbol replTokenParser ":l" <|> Token.symbol replTokenParser ":load") <*> many anyChar

evalCommand :: Parser REPLCommand
evalCommand = Eval <$> many anyChar


replCommand :: Parser REPLCommand
replCommand = quitCommand <|> loadCommand <|> evalCommand
