module Main where

import Evaluator

exprs = [
    "+ 1 2"
    , "+ \"I said, \\\"hello, \" \"world!!\\\"\""
    , "'a' 'b' ' ' '0' '\n'"
    , "def foo ( a b c) ( a + b + c )"
    , "def foo (a b c) a + b + c"
    , "[1, 2, a, b, True, (+ 1 2),  2.35, -2, -35.46 ]"
    ]

main :: IO ()
main = do
    putStrLn "Testing..."
    tests exprs


tests :: [String] -> IO ()
tests (x:xs) = do
    putStrLn $ interpret x
    tests xs
tests [] = return ()

test :: String -> IO ()
test s = putStrLn $ interpret s
