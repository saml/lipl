module CoreLib where

import qualified Control.Monad.Error as E

import LangData

primitives :: [(String, [Val] -> CanBeErr Val)]
primitives = [
    ("+", opAdd)
    , ("-", opSub)
    , ("*", opMult)
    , ("/", floatOpDiv)
    , ("div", intOpDiv)
    , ("&&", boolBinOp (&&))
    , ("||", boolBinOp (||))
    , ("not", boolNot)
    ]

{-
numBinOp op [a,b] = do
    let
        a' = unpackNum a
        b' = unpackNum b
        result = a' `op` b'
    return $
-}

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


opAdd [Int a, Int b] = return $ Int (a + b)
opAdd [Float a, Float b] = return $ Float (a + b)
opAdd [Int a, Float b] = return $ Float (fromIntegral a + b)
opAdd [Float a, Int b] = return $ Float (a + fromIntegral b)
opAdd x = E.throwError $ ArityErr 2 x

opSub [Int a, Int b] = return $ Int (a - b)
opSub [Float a, Float b] = return $ Float (a - b)
opSub [Int a, Float b] = return $ Float (fromIntegral a - b)
opSub [Float a, Int b] = return $ Float (a - fromIntegral b)
opSub x = E.throwError $ ArityErr 2 x

opMult [Int a, Int b] = return $ Int (a * b)
opMult [Float a, Float b] = return $ Float (a * b)
opMult [Int a, Float b] = return $ Float (fromIntegral a * b)
opMult [Float a, Int b] = return $ Float (a * fromIntegral b)
opMult x = E.throwError $ ArityErr 2 x

floatOpDiv [Int a, Int b] = return
    $ Float (fromIntegral a / fromIntegral b)
floatOpDiv [Float a, Float b] = return $ Float (a / b)
floatOpDiv [Int a, Float b] = return $ Float (fromIntegral a / b)
floatOpDiv [Float a, Int b] = return $ Float (a / fromIntegral b)
floatOpDiv x = E.throwError $ ArityErr 2 x

intOpDiv [Int a, Int b] = return $ Int (div a b)
intOpDiv [Float a, Int b] = return $ Int (div (floor a) b)
intOpDiv [Int a, Float b] = return $ Int (div a (floor b))
intOpDiv [Float a, Float b] = return
    $ Int (div (floor a) (floor b))
intOpDiv x = E.throwError $ ArityErr 2 x

boolBinOp op [Bool a, Bool b] = return $ Bool (a `op` b)
boolBinOp op [Bool _, x] =
    E.throwError $ TypeErr "Want Bool there" x
boolBinOp op [x, Bool _] =
    E.throwError $ TypeErr "Want Bool there" x
boolBinOp op x = E.throwError $ ArityErr 2 x

boolNot [Bool a] = return $ Bool (not a)
boolNot [x] = E.throwError $ TypeErr "Want 1 Bool" x
boolNot [] = E.throwError $ ArityErr 1 []
