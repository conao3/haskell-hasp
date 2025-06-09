module Main where

import System.IO

input_ :: IO String
input_ = do
    putStr "> "
    hFlush stdout
    getLine

read_ :: String -> Maybe String
read_ "" = Nothing
read_ s  = Just s

eval_ :: Maybe String -> Maybe String
eval_ Nothing  = Nothing
eval_ (Just s) = Just s

print_ :: Maybe String -> IO ()
print_ Nothing  = pure ()
print_ (Just s) = putStrLn s

repl :: IO ()
repl = do
    inpt <- input_
    print_ (eval_ (read_ inpt))
    repl

main :: IO ()
main = repl
