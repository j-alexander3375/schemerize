module Main (main) where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char (digitToInt)
import Data.Ratio ((%), numerator, denominator)
import Data.Complex (Complex(..))
import Numeric (readOct, readHex, readFloat)
import Data.Array (Array, listArray, elems)
import Control.Monad.Except
import Control.Monad



-- Helper Data
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              -- type predicates (R5RS 6.1 - 6.3)
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("char?", unaryOp isChar),
              ("boolean?", unaryOp isBoolean),
              ("number?", unaryOp isNumber),
              ("complex?", unaryOp isNumber),
              ("real?", unaryOp isReal),
              ("rational?", unaryOp isRational),
              ("integer?", unaryOp isInteger),
              ("list?", unaryOp isList),
              ("pair?", unaryOp isPair),
              ("null?", unaryOp isNull),
              ("vector?", unaryOp isVector),
              -- symbol handling (R5RS 6.3.3)
              ("symbol->string", unaryOpM symbolToString),
              ("string->symbol", unaryOpM stringToSymbol)]

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


-- Error Handling
instance Show LispError where show = showError

type ThrowsError = Either LispError

showError :: LispError -> String 
showError (UnboundVar message varName)  = message ++ ": " ++ varName
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse Error at " ++ show parseErr 
showError (Default msg)                 = "Error: " ++ msg

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractVal :: ThrowsError a -> a
extractVal (Right val) = val
extractVal (Left err)  = error $ "extractVal: unhandled error: " ++ show err

-- Helper Parse Functions

-- Accepts an escape sequence (\", \n, \r, \t, \\) or any character that
-- isn't a quote or the start of an escape sequence, so parseString can
-- support R5RS-style escaping of quotes inside string literals.
escapedChar :: Parser Char
escapedChar = do
                char '\\'
                x <- oneOf "\\\"nrt"
                return $ case x of
                            '\\' -> '\\'
                            '"'  -> '"'
                            'n'  -> '\n'
                            'r'  -> '\r'
                            't'  -> '\t'
                            _    -> x

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapedChar <|> noneOf "\"\\")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

-- Exercise: rewrite parseNumber without liftM.
-- (a) using do-notation
-- _parseNumberDo :: Parser LispVal
-- _parseNumberDo = do
--                    digits <- many1 digit
--                    return $ Number (read digits)

-- -- (b) using explicit sequencing with the >>= operator
-- _parseNumberBind :: Parser LispVal
-- _parseNumberBind = many1 digit >>= return . Number . read

-- Exercise: left-factor list parsing instead of using try.
-- Both (a b c) and (a b . c) start with a run of expressions, so that
-- common prefix gets its own parser, followed by an optional ". expr" tail.
parseExprs :: Parser [LispVal]
parseExprs = sepEndBy parseExpr spaces

parseDottedTail :: Parser (Maybe LispVal)
parseDottedTail = optionMaybe (char '.' >> spaces >> parseExpr)

-- Combines the two halves into a List or DottedList. A tail that is itself
-- a list gets flattened, so (a . (b c)) is List [a, b, c] and
-- (a . (b . c)) is DottedList [a, b] c.
makeList :: [LispVal] -> Maybe LispVal -> Parser LispVal
makeList xs Nothing                    = return $ List xs
makeList [] (Just _)                   = fail "expected an expression before '.'"
makeList xs (Just (List ys))           = return $ List (xs ++ ys)
makeList xs (Just (DottedList ys end)) = return $ DottedList (xs ++ ys) end
makeList xs (Just end)                 = return $ DottedList xs end

parseList :: Parser LispVal
parseList = do
              char '('
              skipMany space
              heads <- parseExprs
              end <- parseDottedTail
              skipMany space
              char ')'
              makeList heads end

-- Exercise: support vectors, eg. #(1 2 3)
-- Stored as an immutable Array for constant-time indexing.
parseVector :: Parser LispVal
parseVector = do
                try (string "#(")
                skipMany space
                xs <- parseExprs
                char ')'
                return $ Vector (listArray (0, length xs - 1) xs)

-- Exercise: support the quote/backquote syntactic sugar (R5RS 4.2.6).
--   'x  -> (quote x)          `x  -> (quasiquote x)
--   ,x  -> (unquote x)        ,@x -> (unquote-splicing x)
quoteForm :: String -> Parser LispVal
quoteForm name = parseExpr >>= \x -> return $ List [Atom name, x]

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> quoteForm "quote"

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = char '`' >> quoteForm "quasiquote"

parseUnquoted :: Parser LispVal
parseUnquoted = char ',' >> ((char '@' >> quoteForm "unquote-splicing")
                             <|> quoteForm "unquote")

-- Interprets a string of '0'/'1' characters as a binary Integer, since
-- Numeric doesn't expose a readBin the way it does readOct/readHex.
readBin :: String -> Integer
readBin = foldl' (\acc c -> acc * 2 + toInteger (digitToInt c)) 0

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

-- Exercise: support the Scheme radix prefixes #b, #o, #d, #x.
parseRadixNumber :: Parser LispVal
parseRadixNumber = do
                      char '#'
                      base <- oneOf "bodx"
                      case base of
                        'b' -> many1 (oneOf "01") >>= return . Number . readBin
                        'o' -> many1 octDigit >>= return . Number . fst . head . readOct
                        'd' -> many1 digit >>= return . Number . read
                        'x' -> many1 hexDigit >>= return . Number . fst . head . readHex
                        _   -> fail "invalid radix"

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseRadixNumber

-- Exercise: support R5RS decimal syntax, eg. 3.14
parseFloat :: Parser LispVal
parseFloat = do
               whole <- many1 digit
               char '.'
               frac <- many1 digit
               return $ Float (fst . head $ readFloat (whole ++ "." ++ frac))

-- Exercise: support Rationals, eg. 3/4
parseRatio :: Parser LispVal
parseRatio = do
               num <- many1 digit
               char '/'
               den <- many1 digit
               return $ Ratio (read num % read den)

-- Exercise: support Complex numbers, eg. 3.0+4.0i or 3+4i
parseComplex :: Parser LispVal
parseComplex = do
                 realPart <- try parseFloat <|> parseDecimal
                 char '+'
                 imagPart <- try parseFloat <|> parseDecimal
                 char 'i'
                 return $ Complex (toDouble realPart :+ toDouble imagPart)
  where
    toDouble (Float f)  = f
    toDouble (Number n) = fromIntegral n
    toDouble _          = error "toDouble: not a real number"

-- Exercise: support R5RS character literals, eg. #\a #\space #\newline
parseCharacter :: Parser LispVal
parseCharacter = do
                    string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar; notFollowedBy alphaNum; return [x] }
                    return $ Character $ case value of
                                            "space"   -> ' '
                                            "newline" -> '\n'
                                            _         -> head value

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _  []            = throwError $ NumArgs 2 []
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- Exercise: no weak typing. Strings like "2" and singleton lists like (2)
-- are no longer coerced; anything that isn't a Number is 0.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                                        if null parsed
                                            then throwError $ TypeMismatch "number" $ String n
                                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum 

-- Applies a one-argument primitive, throwing NumArgs on the wrong arity.
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f = unaryOpM (return . f)

-- Like unaryOp, but for primitives that can fail on their argument.
unaryOpM :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOpM f [v]  = f v
unaryOpM _ args = throwError $ NumArgs 1 args

-- Exercise: type-testing primitives.
isSymbol, isString, isChar, isBoolean, isNumber, isReal, isRational,
  isInteger, isList, isPair, isNull, isVector :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isString (String _) = Bool True
isString _          = Bool False

isChar (Character _) = Bool True
isChar _             = Bool False

isBoolean (Bool _) = Bool True
isBoolean _        = Bool False

-- Every number we can represent is a complex number, so number? and
-- complex? are the same test.
isNumber (Number _)  = Bool True
isNumber (Float _)   = Bool True
isNumber (Ratio _)   = Bool True
isNumber (Complex _) = Bool True
isNumber _           = Bool False

isReal (Complex _) = Bool False
isReal v           = isNumber v

-- Finite floats are rational in R5RS, eg. (rational? 0.5) => #t
isRational (Float f) = Bool $ not (isNaN f || isInfinite f)
isRational (Complex _) = Bool False
isRational v           = isNumber v

-- (integer? 3.0) => #t, so integral floats count too.
isInteger (Number _) = Bool True
isInteger (Float f)  = Bool $ not (isInfinite f) && f == fromInteger (round f)
isInteger _          = Bool False

-- Only proper lists; (a . b) is a pair but not a list.
isList (List _) = Bool True
isList _        = Bool False

isPair (List (_:_))      = Bool True
isPair (DottedList _ _)  = Bool True
isPair _                 = Bool False

isNull (List []) = Bool True
isNull _         = Bool False

isVector (Vector _) = Bool True
isVector _          = Bool False

-- Exercise: symbol-handling primitives. A symbol is an Atom.
symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom name) = return $ String name
symbolToString notSymbol   = throwError $ TypeMismatch "symbol" notSymbol

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notString  = throwError $ TypeMismatch "string" notString

-- Parse
parseExpr :: Parser LispVal
parseExpr = parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseCharacter
        <|> parseVector
        <|> parseAtom
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnquoted
        <|> parseList

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

-- Helper for expression handling
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Basic expression handling
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ concatMap escapeChar contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio r) = show (numerator r) ++ "/" ++ show (denominator r)
showVal (Complex (r :+ i)) = show r ++ (if i < 0 then "-" else "+") ++ show (abs i) ++ "i"
showVal (Character c) = "#\\" ++ case c of
                                     ' '  -> "space"
                                     '\n' -> "newline"
                                     _    -> [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector arr) = "#(" ++ unwordsList (elems arr) ++ ")"

-- Reverses escapedChar, so a printed string can be read back in.
escapeChar :: Char -> String
escapeChar '"'  = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar c    = [c]

-- Eval
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Character _) = return val
eval val@(Vector _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised Special Form" badForm

-- Apply
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func)
                        ($ args)
                        (lookup func primitives)


-- Main
main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractVal $ trapError evaled 