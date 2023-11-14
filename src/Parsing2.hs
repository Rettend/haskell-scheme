module Parsing2 where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  deriving (Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseEscape <|> noneOf "\"")
  char '"'
  return $ String x

-- | Exercise 2
-- kinda not right
parseEscape :: Parser Char
parseEscape = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'

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

-- | Exercise 1.1
-- parseNumber = do
--   num <- many1 digit
--   return $ Number (read num)

-- >>> show <$> Just 3
-- Just "3"

-- | Exercise 1.2
-- parseNumber = many1 digit >>= (return . Number . read)
parseNumber = Number . read <$> many1 digit

-- | Exercise 6
parseFloat :: Parser LispVal
parseFloat = do
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  return $ Float (read (whole ++ "." ++ decimal))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBackquoted
    <|> try parseUnquoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

-- | Exercise 1.1
parseBackquoted :: Parser LispVal
parseBackquoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]
