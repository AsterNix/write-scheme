module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

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
         <|> parseNumber
         <|> parseFloat
         <|> parseChar
         <|> parseBool
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> (parseDottedList)
                char ')'
                return x


--Evaluator begins here
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", oneOp testAtom),
              ("string?", oneOp testString),
              ("number?", oneOp testNumber),
              ("symbol->string", oneOp atomToString),
              ("string->symbol", oneOp stringToAtom)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

oneOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
oneOp op  [n] = op n
oneOp op _ = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--comment out the block to make everything not a 
{-unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n-}
unpackNum _ = 0

testAtom :: LispVal -> LispVal
testAtom (Atom _) = Bool True
testAtom _ = Bool False

testString :: LispVal -> LispVal
testString (String _) = Bool True
testString _ = Bool False

testNumber :: LispVal -> LispVal
testNumber (Number _) = Bool True
testNumber _ = Bool False
  
atomToString :: LispVal -> LispVal
atomToString (Atom a) = String a

stringToAtom :: LispVal -> LispVal
stringToAtom (String s) = Atom s

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ " args; found values "
                                          ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parser error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
