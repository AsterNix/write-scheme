module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Float

escapeChars :: Parser Char
escapeChars = do char '\\'
                 x <- oneOf "\\\"nrt"
                 return $ case x of 
                   '\\' -> x
                   '"' -> x
                   'n' -> '\n'
                   'r' -> '\r'
                   't' -> '\t'
                   
parseString :: Parser LispVal
parseString = do char '"'
--                 x <- many (noneOf "\"")
                 x <- many $ escapeChars <|> noneOf"\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

numDec1 :: Parser LispVal
numDec1 = do n <- many1 digit
             return (Number (read(n)))

numDec2 :: Parser LispVal
numDec2 = do try $ string "#d"
             x <- many1 digit
             (return . Number . read) x

hex2dig x = fst $ readHex x !! 0 
numHex :: Parser LispVal
numHex = do try $ string "#x"
            x <- many1 hexDigit
            (return . Number . hex2dig) x

oct2dig x = fst $ readOct x !! 0
numOct :: Parser LispVal
numOct = do try $ string "#o"
            x <- many1 octDigit
            (return . Number . oct2dig) x

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
numBin :: Parser LispVal
numBin = do try $ string "#b"
            x <- many1 (oneOf "10")
            return $ Number (bin2dig x)
             
parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = numDec1 <|> numDec2 <|> numHex <|> numOct <|> numBin

parseBool :: Parser LispVal
parseBool = do char '#'
               (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

charSpace :: Parser LispVal
charSpace = do try $ string "#\\space"
               return $ Char ' '
charSpace2 :: Parser LispVal
charSpace2 = do try $ string "#\\"
                return $ Char ' '               
charNewline :: Parser LispVal
charNewline = do try $ string "#\\newline"
                 return $ Char '\n'
charLetter :: Parser LispVal
charLetter = do try $ string "#\\"
                c <- letter
                return $ Char c

parseChar :: Parser LispVal
parseChar = charSpace
         <|> charSpace2
         <|> charNewline
         <|> charLetter

float2dig x = fst $ readFloat(x) !! 0
parseFloat :: Parser LispVal
parseFloat = do w <- many(digit) --whole part
                char '.'
                f <- many(digit) --fractional part
                return (Float(float2dig(w ++ '.':f)))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- (char '.' >> spaces >> parseExpr)
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseBacktick :: Parser LispVal
parseBacktick = do char '`'
                   x <- parseExpr
                   return $ List [Atom "quasiquote", x]
                   
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseFloat
         <|> parseNumber
         <|> parseChar
         <|> parseBool
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> (parseDottedList)
                char ')'
                return x
