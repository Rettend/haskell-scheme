module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- spaces :: Parser ()
-- spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- parseNumber = do
--   num <- many1 digit
--   return $ Number (read num)

-- parseNumber = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)