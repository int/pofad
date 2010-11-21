import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language
import Data.Char

-- this is my quick & dirty version
-- get all arrangments -> parsec -> filter
make :: [Integer] -> [String]
make [] = []
make [x] = [show x]
make (x:xs) = concat [ [s ++ y, s ++ " + " ++ y, s ++ "*" ++ y] | y <- make xs ]
    where s = show x

get :: String -> Integer
get s = case parse expr "" s of
             Right i -> i

solutions :: Integer -> [Integer] -> [String]
solutions ans = filter ((==ans) . get . strip) . make
    where strip = filter (/= ' ')

-- parser
lexer = makeTokenParser emptyDef
expr = buildExpressionParser table (decimal lexer)
table = [[op "*" (*) AssocLeft], [op "+" (+) AssocLeft]]
    where op s f = Infix (do {string s; return f})

-- print helper
doit ans digits = do
    putStrLn $ "get " ++ s ++ " from " ++ show digits ++ ":\n"
    mapM_ (putStrLn . f) $ solutions ans digits
    putStrLn ""
    where s = show ans
          f x = s ++ " = " ++ x

-- some examples
main = do
    doit 100 [1..9]
    doit 1234 $ map (fromIntegral . digitToInt) $ take 12 . drop 2 $ show pi
