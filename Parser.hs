{-# LANGUAGE ExistentialQuantification #-}
module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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


--Evaluator begins here--
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do result <- eval pred
     case result of
       Bool False -> eval alt
       Bool True  -> eval conseq
       notBool -> throwError $ TypeMismatch "bool" notBool
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized spcial form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
              ("string->symbol", oneOp stringToAtom),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

oneOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
oneOp op  [n] = return $ op n
oneOp op mult = throwError $ NumArgs 1 mult

--           unpacker func                 eval func           input        result
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n  
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $
                                             (length arg1 == length arg2) &&
                                             (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False --currently never called
                               Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


--this defines a "type hider" that facilitates the creation of a list of
--unpacker functions that take a LispVal to an arbitrary type
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
         `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList      



--Error handling begins here--

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
