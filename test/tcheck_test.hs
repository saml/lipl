{-# OPTIONS_GHC -i../src #-}

module TCheckTest where

import TCheck
import TParse
import Type
import Main
import Parser

ty input = case parseSingle input of
    Left err -> error (show err)
    Right val -> do
        t <- typeInfer val
        return t

{-
runTests = [
    ty "(let { f = (lambda (f) f) } ( (f 1), (f True)))"
        `tEq` tParse "(Int,Bool)"
    , ty "(let { x = (lambda (x) (+ 1 x)) } (x 1))"
        `tEq` tInt
    , ty "(let { x = (lambda (x) (if (== 1 (head x)) 'a' 'b')), y = x} (y ))"
        `tEq` tParse "[Int] -> Char"
    , ty "(lambda (f x) (f x))"
        `tEq` tParse "(t0 -> t1) -> t0 -> t1"
    , ty "(def fac (n) (if (<= n 0) 1 (* n (fac (- n 1)))))"
        `tEq` tParse "Int -> Int"
    , ty "(def twice (f x) (f (f x)))"
        `tEq` tParse "(t0 -> t0) -> t0 -> t0"
    , ty "(def len (l) (if (isEmpty l) 0 (+ 1 (len (tail l)))))"
        `tEq` tParse "[t0] -> Int"
    , ty "(def filter (f l) (if (f (head l)) (cons (head l) (filter f (tail l))) (filter f (tail l))))"
        `tEq` tParse "(t0 -> Bool) -> [t0] -> [t0]"
    , ty "(def map (f l) (cons (f (head l)) (map f (tail l))))"
        `tEq` tParse "(t0 -> t1) -> [t0] -> [t1]"
    , ty "(def map (f l) (let {x = (head l), xs = (map f (tail l))} (cons (f x) xs)))"
        `tEq` tParse "(t0 -> t1) -> [t0] -> [t1]"
    , ty "(lambda (l) (let {x = (head l), xs = (tail l)} (cons x xs)))"
        `tEq` tParse "[t0] -> [t0]"
    , ty "(lambda (l) (let {x = (head l), xs = (tail l)} (x,xs)))"
        `tEq` tParse "[t0] -> (t0, [t0])"
    , ty "((lambda (f x y) (f y x)) (lambda (x) x))"
        `tEq` tParse "b -> (b -> c) -> c"
    , ty "(lambda (x) (let {x = 1} x))"
        `tEq` tParse "a -> Int"
    , ty "(lambda (x) (lambda (x) (+ x x)))"
        `tEq` tParse "(t0 -> Int -> Int)"
    , ty "(def acc (f i l) (if (isEmpty l) i (f (head l) (acc f i (tail l)))))"
        `tEq` tParse "(t0 -> t1 -> t1) -> t1 -> [t0] -> t1"
    , ty "(lambda (f x y) (f y x))"
        `tEq` tParse "(a -> b -> c) -> b -> a -> c"
    , ty "(lambda (x) (let { x = 1 } (+ x ((lambda (x) x) x))))"
        `tEq` tParse "t0 -> Int"
    , ty "(def map (f l) (if (isEmpty l) [] (let {x = (head l), xs = (tail l)} (cons (f x) (map f xs)))))"
        `tEq` tParse "(t0 -> t1) -> [t0] -> [t1]"
    , ty "(lambda (x) ((let { x = (lambda (x) (+. 0.1 x)) } x) x))"
        `tEq` tParse "Float -> Float"
    ]
    -}
