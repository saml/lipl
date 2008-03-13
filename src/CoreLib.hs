module CoreLib where

import qualified Control.Monad.Error as E

import LangData

funcall :: String -> [Val] -> EvalVal Val
funcall fname args = case lookup fname primitives of
    Nothing -> E.throwError
        $ NotFunErr "Unrecognizable primitive function" fname
    Just f -> f args

primitives :: [(String, [Val] -> EvalVal Val)]
primitives = [
    ("+", opAdd)
    , ("-", opSub)
    , ("*", opMult)
    , ("/", floatOpDiv)
    , ("div", intOpDiv)
    , ("&&", boolBinOp (&&))
    , ("||", boolBinOp (||))
    , ("not", boolNot)
    , ("==", compareEq)
    , ("!=", compareNeq)
    , ("<", compareLt)
    , ("<=", compareLte)
    , (">", compareGt)
    , (">=", compareGte)
    , ("head", listHead)
    , ("tail", listTail)
    , ("cons", listCons)
    ]

compareEq = compareOp (ordBool [EQ])
compareNeq = compareOp (ordBool [LT, GT])
compareLt = compareOp (ordBool [LT])
compareLte = compareOp (ordBool [LT, EQ])
compareGt = compareOp (ordBool [GT])
compareGte = compareOp (ordBool [GT, EQ])

ordBool target x = elem x target

--compareOp op [a, b] =
--    return $ Bool $ op (unpackVal a `compare` unpackVal b)
compareOp op [Int a, Int b] = return $ Bool $ op (compare a b)
compareOp op [Float a, Int b] =
    return $ Bool $ op (compare a (fromIntegral b))
compareOp op [Int a, Float b] =
    return $ Bool $ op (compare (fromIntegral a) b)
compareOp op [Float a, Float b] = return $ Bool $ op (compare a b)
compareOp op [Bool a, Bool b] = return $ Bool $ op (compare a b)
compareOp op [Str a, Str b] = return $ Bool $ op (compare a b)
compareOp op [Char a, Char b] = return $ Bool $ op (compare a b)
compareOp op [List a, List b] = return $ Bool $ op (compare a b)
compareOp op e@([a,b]) =
    E.throwError $ TypeErr "Can't compare" $ Expr e

{-
compareOp unpacker op [a,b] = do
    return $ Bool (unpacker a `op` unpacker b)
compareOp unpacker op x = E.throwError $ ArityErr 2 x

numCompareOp = compareOp unpackInt

unpackInt :: Val -> CanBeErr Integer
unpackInt (Int i) = return i
unpackInt (Expr [i]) = unpackInt i

unpackFloat (Float f) = return f
unpackFloat (Expr [f]) = unpackFloat f

unpackBool (Bool b) = return b
unpackBool (Expr [b]) = unpackBool b
-}

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

floatOpDiv x@([Int a, Int b]) =
    E.throwError $ TypeErr "2 Floats" (Expr x)
floatOpDiv [Float a, Float b] = return $ Float (a / b)
floatOpDiv [Int a, Float b] = return $ Float (fromIntegral a / b)
floatOpDiv [Float a, Int b] = return $ Float (a / fromIntegral b)
floatOpDiv x = E.throwError $ ArityErr 2 x

intOpDiv [Int a, Int b] = return $ Int (div a b)
intOpDiv [Float a, Int b] = return $ Int (div (floor a) b)
intOpDiv [Int a, Float b] = return $ Int (div a (floor b))
intOpDiv x@([Float a, Float b]) =
    E.throwError $ TypeErr "2 Ints" (Expr x)
intOpDiv x = E.throwError $ ArityErr 2 x

boolBinOp op [Bool a, Bool b] = return $ Bool (a `op` b)
boolBinOp op x@([Bool _, _]) =
    E.throwError $ TypeErr "Bool for 2nd arg" (Expr x)
boolBinOp op x@([_, Bool _]) =
    E.throwError $ TypeErr "Bool for 1st arg" (Expr x)
boolBinOp op x = E.throwError $ ArityErr 2 x

boolNot [Bool a] = return $ Bool (not a)
boolNot [x] = E.throwError $ TypeErr "Bool" x
boolNot [] = E.throwError $ ArityErr 1 []

listHead [List (x:xs)] = return x
listHead [e@(List [])] =
    E.throwError $ TypeErr "non empty list" e
listHead [x] = E.throwError $ TypeErr "non empty list" x
listHead x = E.throwError $ ArityErr 1 x

listTail [List (x:xs)] = return $ List xs
listTail [e@(List [])] =
    E.throwError $ TypeErr "non empty list" e
listTail [x] = E.throwError $ TypeErr "non empty list" x
listTail x = E.throwError $ ArityErr 1 x

listCons [x, List []] = return $ List [x]
listCons [x, List xs] = return $ List (x:xs)
listCons x = E.throwError $ ArityErr 2 x
