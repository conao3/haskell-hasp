module Main where

import System.IO

input_ :: IO String
input_ = do
    putStr "> "
    hFlush stdout
    getLine

read_ :: String -> String
read_ = id

eval_ :: String -> String
eval_ = id

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
    inpt <- input_
    print_ (eval_ (read_ inpt))
    repl

main :: IO ()
main = repl
