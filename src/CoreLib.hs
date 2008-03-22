module CoreLib where

import qualified Control.Monad.Error as E
import qualified Control.Monad.Trans as T
import qualified Data.Map as Map

import LangData

data Builtin = Builtin {
    getBuiltinArity :: Int
    , getBuiltinFun :: [Val] -> Wrap Val
    }

funcall :: String -> [Val] -> Wrap Val
funcall fname args = case lookup fname primitives of
    Nothing -> E.throwError
        $ NotFunErr "Unrecognizable primitive function" fname
    Just f -> (getBuiltinFun f) args

arityOf name = case lookup name primitives of
    Nothing -> (-1)
    Just f -> getBuiltinArity f

primitives :: [(String, Builtin)]
primitives = [
    ("+", Builtin 2 opAdd)
    , ("-", Builtin 2 opSub)
    , ("*", Builtin 2 opMult)
    , ("/", Builtin 2 floatOpDiv)
    , ("div", Builtin 2 intOpDiv)
    , ("&&", Builtin 2 $ boolBinOp (&&))
    , ("||", Builtin 2 $ boolBinOp (||))
    , ("not", Builtin 1 boolNot)
    , ("==", Builtin 2 compareEq)
    , ("!=", Builtin 2 compareNeq)
    , ("<", Builtin 2 compareLt)
    , ("<=", Builtin 2 compareLte)
    , (">", Builtin 2 compareGt)
    , (">=", Builtin 2 compareGte)
    , ("length", Builtin 1 listLength)
    , ("head", Builtin 1 listHead)
    , ("tail", Builtin 1 listTail)
    , ("cons", Builtin 2 listCons)
    , ("isEmpty", Builtin 1 listIsEmpty)
    , ("println", Builtin 1 printVarLn)
    , ("show", Builtin 1 showVar)
    , ("env", Builtin 1 showEnvironment)
    , ("from-str", Builtin 1 strToList)
    , ("from-list", Builtin 1 listToStr)
    ]

builtinNames = map fst primitives
builtins = map snd primitives


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
opAdd x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
opAdd x = E.throwError $ ArityErr 2 x

opSub [Int a, Int b] = return $ Int (a - b)
opSub [Float a, Float b] = return $ Float (a - b)
opSub [Int a, Float b] = return $ Float (fromIntegral a - b)
opSub [Float a, Int b] = return $ Float (a - fromIntegral b)
opSub x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
opSub x = E.throwError $ ArityErr 2 x

opMult [Int a, Int b] = return $ Int (a * b)
opMult [Float a, Float b] = return $ Float (a * b)
opMult [Int a, Float b] = return $ Float (fromIntegral a * b)
opMult [Float a, Int b] = return $ Float (a * fromIntegral b)
opMult x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
opMult x = E.throwError $ ArityErr 2 x

floatOpDiv x@([Int a, Int b]) =
    E.throwError $ TypeErr "2 Floats" (Expr x)
floatOpDiv [Float a, Float b] = return $ Float (a / b)
floatOpDiv [Int a, Float b] = return $ Float (fromIntegral a / b)
floatOpDiv [Float a, Int b] = return $ Float (a / fromIntegral b)
floatOpDiv x@([_, _]) = E.throwError $ TypeErr "Float" (Expr x)
floatOpDiv x = E.throwError $ ArityErr 2 x

intOpDiv [Int a, Int b] = return $ Int (div a b)
intOpDiv [Float a, Int b] = return $ Int (div (floor a) b)
intOpDiv [Int a, Float b] = return $ Int (div a (floor b))
intOpDiv x@([Float a, Float b]) =
    E.throwError $ TypeErr "2 Ints" (Expr x)
intOpDiv x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
intOpDiv x = E.throwError $ ArityErr 2 x

boolBinOp op [Bool a, Bool b] = return $ Bool (a `op` b)
boolBinOp op x@([Bool _, _]) =
    E.throwError $ TypeErr "Bool for 2nd arg" (Expr x)
boolBinOp op x@([_, Bool _]) =
    E.throwError $ TypeErr "Bool for 1st arg" (Expr x)
boolBinOp op x = E.throwError $ ArityErr 2 x

boolNot [Bool a] = return $ Bool (not a)
boolNot [x] = E.throwError $ TypeErr "Bool" x
boolNot x = E.throwError $ ArityErr 1 x

listLength [List x] = return $ Int (toInteger $ length x)
listLength [Str x] = return $ Int (toInteger $ length x)
listLength [x] = E.throwError $ TypeErr "need list" x
listLength x = E.throwError $ ArityErr 1 x

listHead [List (x:xs)] = return x
listHead [e@(List [])] =
    E.throwError $ TypeErr "need non empty list" e
listHead [Str (x:xs)] = return $ Char x
listHead [e@(Str "")] =
    E.throwError $ TypeErr "need non empty string" e
listHead [x] = E.throwError $ TypeErr "need non empty list" x
listHead x = E.throwError $ ArityErr 1 x

listTail [List (x:xs)] = return $ List xs
listTail [e@(List [])] =
    E.throwError $ TypeErr "need non empty list" e
listTail [Str (x:xs)] = return $ Str xs
listTail [e@(Str [])] =
    E.throwError $ TypeErr "need non empty string" e
listTail [x] = E.throwError $ TypeErr "need non empty list" x
listTail x = E.throwError $ ArityErr 1 x

listCons [x, List []] = return $ List [x]
listCons [x, List xs] = return $ List (x:xs)
listCons [Char x, Str ""] = return $ Str [x]
listCons [Char x, Str xs] = return $ Str (x:xs)
listCons x = E.throwError $ ArityErr 2 x

listIsEmpty [List a] = return $ Bool (null a)
listIsEmpty [Str a] = return $ Bool (null a)
listIsEmpty [x] = E.throwError $ TypeErr "need list" x
listIsEmpty x = E.throwError $ ArityErr 1 x

printVarLn [Str x] = do
    T.liftIO $ putStrLn x
    return Null
printVarLn [x] = E.throwError $ TypeErr "need string" x
printVarLn x = E.throwError $ ArityErr 1 x

{-
concatStr [Str s1, Str s2] = return $ Str (s1 ++ s2)
concatStr x@([_, _]) = E.throwError $ TypeErr "need string" (Expr x)
concatStr x = E.throwError $ ArityErr 2 x
-}

showVar [x] = return $ Str (show x)
showVar x = E.throwError $ ArityErr 1 x

showEnvironment [Str key] = do
    env <- getEnv
    if key == ""
        then
            return $ Str (show env)
        else
            do
                val <- Map.lookup key env
                return val

strToList [Str s] = return (List $ map Char s)
strToList [x] = E.throwError $ TypeErr "need string" x
strToList x = E.throwError $ ArityErr 1 x

listToStr [List l] = do
    s <- mapM toChar l
    return $ Str s
    where
        toChar (Char c) = return c
        toChar x = E.throwError $ TypeErr "need list of chars" x
listToStr [x] = E.throwError $ TypeErr "need list" x
listToStr x = E.throwError $ ArityErr 1 x
