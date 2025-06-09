module Main where

import System.IO
import Data.Char (isSpace, isDigit)

data HaspExp = Integer Integer
               | Symbol String
               | List [HaspExp]
               deriving (Eq, Show)

input_ :: IO String
input_ = do
    putStr "> "
    hFlush stdout
    getLine

parseValue :: String -> HaspExp
parseValue s
    | all isDigit s = Integer (read s)
    | otherwise     = Symbol s

tokenize :: String -> String
tokenize [] = []
tokenize ('(':xs) = " ( " ++ tokenize xs
tokenize (')':xs) = " ) " ++ tokenize xs
tokenize (x:xs) = x : tokenize xs

read_ :: String -> Maybe HaspExp
read_ s
    | all isSpace s = Nothing
    | otherwise     = case words (tokenize s) of
        []     -> Nothing
        [x]    -> Just (parseValue x)
        xs     -> Just (List (map parseValue xs))

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
