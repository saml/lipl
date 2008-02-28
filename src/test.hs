module Main where

import Evaluator

exprs = [
    "(+ 1 2)"
    , "(+ \"I said, \\\"hello, \" \"world!!\\\"\")"
    , "(foo 'a' 'b') (bar ' ' '0' '\n')"
    , "(def foo ( a b c) ( a + b + c ))"
    , "(def foo (a b c) a + b + c )"
    , "(foo [1, 2, a, b, True, (+ 1 2),  2.35, -2 , -35.46 ])"
    , "(if True 1 (if False 1 2))"
    , "(let {a = 1, b = 2} (+ a b))"
    ]

main :: IO ()
main = do
    putStrLn "Testing..."
    --interact interpret
    tests exprs


tests :: [String] -> IO ()
tests (x:xs) = do
    test x
    tests xs
tests [] = return ()

test :: String -> IO ()
test s = do
    putStrLn s
    putStrLn $ interpret s
