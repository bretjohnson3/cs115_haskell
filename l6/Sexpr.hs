--
-- S-expression parser.
--
-- Answer to C.3 at bottom ****

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

-- C.5 change datatype
data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String
  | StringA String
  deriving (Show)


-- C.1 -> remove QuoteS
data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"


-- C.4 exponent helper function
parseExpHelper :: Parser [Char]
parseExpHelper = do
    exp <- oneOf "eE"
    sign <- option "" (string "-" <|> string "+")
    digits <- many1 digit
    return ([exp] ++ sign ++ digits)

-- C.4 exponent functionality
parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  exp <- option "" parseExpHelper
  return (read (sign ++ digits ++ "." ++ f ++ exp) :: Double)
  <?> "floating-point number"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

-- C.5 helper
parseString :: Parser String
parseString = do
    char '\"'
    str <- many (noneOf "\"")
    char '\"'
    return str 
    <?> "string"

-- C.5 added string parse after int before id
parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> try (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- C.2 Added helper function for parseList
parseListHelper :: Char -> Char -> Parser [Sexpr]
parseListHelper a b = do 
    char a 
    optional parseSep
    ss <- parseSexpr `sepEndBy` parseSep
    char b 
    return ss 
    <?> "helper for list of S-expressions"

-- C.2 fixed to support other delimeters (,[,{,),],}
-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = do
    parseListHelper '(' ')'
    <|> parseListHelper '[' ']'
    <|> parseListHelper '{' '}'
    <?> "list of S-expressions"

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = char '\'' >> parseSexpr
  <?> "quoted S-expression"


-- C.1 -> remove QuoteS
-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> (parseQuote) 
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '


-- C.1 -> remove QuoteS
-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"


-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"



--- Answer to C.3
{-
We never need to backtrack (and it is expensive for the parser to do so)
so we can leave out the try. 
-}
