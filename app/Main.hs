module Main where

import System.IO

read_ :: IO String
read_ = do
    putStr "> "
    hFlush stdout
    getLine

eval_ :: String -> String
eval_ = id

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
    readed <- read_
    print_ (eval_ readed)
    repl

main :: IO ()
main = repl
