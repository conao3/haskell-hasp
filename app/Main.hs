module Main where

import System.IO
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data HaspExp = Integer Integer
               | Symbol String
               | List [HaspExp]
               | Lambda [String] HaspExp
               deriving (Eq, Show)

type Environment = [(String, HaspExp)]

input_ :: IO String
input_ = do
    putStr "> "
    hFlush stdout
    getLine

atom :: String -> HaspExp
atom token =
    case readMaybe token :: Maybe Integer of
        Just n -> Integer n
        Nothing -> Symbol token

readFromTokens :: [String] -> ([String], HaspExp)
readFromTokens [] = error "unexpected EOF while reading"
readFromTokens (token:tokens)
    | token == "(" =
        let (rest, lst) = readListExp tokens
        in (rest, List lst)
    | token == ")" = error "unexpected )"
    | otherwise = (tokens, atom token)

readListExp :: [String] -> ([String], [HaspExp])
readListExp [] = error "unexpected EOF while reading"
readListExp (")":rest) = (rest, [])
readListExp tokens =
    let (rest1, expr) = readFromTokens tokens
        (rest2, exprs) = readListExp rest1
    in (rest2, expr : exprs)

parse :: String -> HaspExp
parse s = case readFromTokens (tokenize s) of
    ([], expr) -> expr
    (_, _) -> error "Extra tokens after expression"

tokenize :: String -> [String]
tokenize s = words $ concatMap addSpaces s
  where
    addSpaces '(' = " ( "
    addSpaces ')' = " ) "
    addSpaces c = [c]

read_ :: String -> Maybe HaspExp
read_ "" = Nothing
read_ s = case tokenize s of
    [] -> Nothing
    _ -> Just (parse s)

eval' :: Environment -> HaspExp -> (Environment, HaspExp)
eval' env (Integer n) = (env, Integer n)
eval' env (Symbol s) = (env, fromMaybe (error $ "Undefined symbol: " ++ s) (lookup s env))
eval' env (Lambda params body) = (env, Lambda params body)
eval' env (List l) = case l of
    [] -> (env, List [])
    [Symbol "quote", v] -> (env, v)
    [Symbol "if", condition, thenExp, elseExp] ->
        case snd (eval' env condition) of
            Integer 0 -> eval' env elseExp
            _ -> eval' env thenExp
    [Symbol "lambda", List params, body] ->
        let paramNames = map (\p -> case p of
                Symbol s -> s
                _ -> error "Lambda parameters must be symbols") params
        in (env, Lambda paramNames body)
    [Symbol "def", Symbol name, value] ->
        let (_, evaluatedValue) = eval' env value
            newEnv = (name, evaluatedValue) : env
        in (newEnv, evaluatedValue)
    (func:args) ->
        let (_, evaluatedFunc) = eval' env func
            evaluatedArgs = map (snd . eval' env) args
        in case evaluatedFunc of
            Lambda params body ->
                if length params /= length evaluatedArgs
                then error $ "Wrong number of arguments: expected " ++ show (length params) ++ ", got " ++ show (length evaluatedArgs)
                else let newEnv = zip params evaluatedArgs ++ env
                     in eval' newEnv body
            _ -> error $ "Not a function: " ++ show evaluatedFunc

eval_ :: Environment -> Maybe HaspExp -> (Environment, Maybe HaspExp)
eval_ env Nothing = (env, Nothing)
eval_ env (Just v) =
    let (newEnv, result) = eval' env v
    in (newEnv, Just result)

showValue :: HaspExp -> String
showValue (Integer n) = show n
showValue (Symbol s) = s
showValue (List xs) = "(" ++ unwords (map showValue xs) ++ ")"
showValue (Lambda params _) = "#<lambda:" ++ unwords params ++ ">"

print_ :: Maybe HaspExp -> IO ()
print_ Nothing  = pure ()
print_ (Just v) = putStrLn (showValue v)

repl :: Environment -> IO ()
repl env = do
    inpt <- input_
    let (newEnv, result) = eval_ env (read_ inpt)
    print_ result
    repl newEnv

main :: IO ()
main = repl []
