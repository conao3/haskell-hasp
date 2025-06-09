module Main where

import System.IO
import Data.Char (isDigit)
import Text.Read (readMaybe)

data HaspExp = Integer Integer
               | Symbol String
               | List [HaspExp]
               deriving (Eq, Show)

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

eval_ :: Maybe HaspExp -> Maybe HaspExp
eval_ Nothing = Nothing
eval_ (Just v) = Just v

showValue :: HaspExp -> String
showValue (Integer n) = show n
showValue (Symbol s) = s
showValue (List xs) = "(" ++ unwords (map showValue xs) ++ ")"

print_ :: Maybe HaspExp -> IO ()
print_ Nothing  = pure ()
print_ (Just v) = putStrLn (showValue v)

repl :: IO ()
repl = do
    inpt <- input_
    print_ (eval_ (read_ inpt))
    repl

main :: IO ()
main = repl
