module CoreLib where

import LangData

primitives :: [(String, [Val] -> Val)]
primitives = [
    ("+", opAdd)
    , ("-", opSub)
    , ("*", opMult)
    , ("/", opDiv)
    ]

{-
mkOp :: (Num a) => (a -> a -> a) -> [Val] -> Val
mkOp f [Int a, Int b] = Int (f a b)
mkOp f [Float a, Float b] = Float (f a b)
mkOp f [Int a, Float b] = Float (f (fromIntegral a) b)
mkOp f [Float a, Int b] = Float (f a (fromIntegral b))

opAdd = mkOp (+)
opSub = mkOp (-)
opMult = mkOp (*)
opDiv args@([Int _, Int _]) = mkOp div args
opDiv args = mkOp (/) args
-}

-- (opAdd.)(:) :: Val -> [Val] -> Val
-- \x xs -> opAdd (x:xs)
opAdd [Int a, Int b] = Int (a + b)
opAdd [Float a, Float b] = Float (a + b)
opAdd [Int a, Float b] = Float (fromIntegral a + b)
opAdd [Float a, Int b] = Float (a + fromIntegral b)

opSub [Int a, Int b] = Int (a - b)
opSub [Float a, Float b] = Float (a - b)
opSub [Int a, Float b] = Float (fromIntegral a - b)
opSub [Float a, Int b] = Float (a - fromIntegral b)

opMult [Int a, Int b] = Int (a * b)
opMult [Float a, Float b] = Float (a * b)
opMult [Int a, Float b] = Float (fromIntegral a * b)
opMult [Float a, Int b] = Float (a * fromIntegral b)

opDiv [Int a, Int b] = Int (div a b)
opDiv [Float a, Float b] = Float (a / b)
opDiv [Int a, Float b] = Float (fromIntegral a / b)
opDiv [Float a, Int b] = Float (a / fromIntegral b)


