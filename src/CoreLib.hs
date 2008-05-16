{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module CoreLib where

import qualified Control.Monad.Error as E
import qualified Control.Monad.Trans as T
import qualified Data.Map as Map
import System.IO
import Data.Function (fix)

import LangData
import TParse
import Type
import EvalMonad
import Error
import PosMonad

data Builtin = Builtin {
    getBuiltinArity :: Int
    --, getBuiltinFun :: [Val] -> Eval Val
    , getBuiltinFun ::
        forall m .
            (MonadEval m, E.MonadError Err m, T.MonadIO m, MonadPos m)
            => [Val] -> m Val
    , getBuiltinType :: Type
    }

--funcall :: (MonadEval m, E.MonadError Err m) => String -> [Val] -> m Val
funcall fname args = case Map.lookup fname primitives of
    Nothing -> do
        pos <- getSourcePos
        E.throwError
            $ Err pos ("Unrecognizable primitive function: " ++ fname)
    Just f -> (getBuiltinFun f) args

arityOf name = case Map.lookup name primitives of
    Nothing -> (-1)
    Just f -> getBuiltinArity f

--primitives :: [(String, Builtin)]
primitives = Map.fromList [
    ("+", Builtin 2 opAdd (tParse "Int -> Int -> Int"))
    , ("+.", Builtin 2 opAdd (tParse "Float -> Float -> Float"))
    , ("-", Builtin 2 opSub (tParse "Int -> Int -> Int"))
    , ("-.", Builtin 2 opSub (tParse "Float -> Float -> Float"))
    , ("*", Builtin 2 opMult (tParse "Int -> Int -> Int"))
    , ("*.", Builtin 2 opMult (tParse "Float -> Float -> Float"))
    , ("div", Builtin 2 intOpDiv (tParse "Int -> Int -> Int"))
    , ("/", Builtin 2 floatOpDiv (tParse "Float -> Float -> Float"))
    , ("toInt", Builtin 1 toInt (tParse "Float -> Int"))
    , ("toFloat", Builtin 1 toFloat (tParse "Int -> Float"))
    , ("&&", Builtin 2 (boolBinOp (&&)) (tParse "Bool -> Bool -> Bool"))
    , ("||", Builtin 2 (boolBinOp (||)) (tParse "Bool -> Bool -> Bool"))
    , ("not", Builtin 1 boolNot (tParse "Bool -> Bool"))
    , ("==", Builtin 2 compareEq (tParse "a -> a -> Bool"))
    , ("!=", Builtin 2 compareNeq (tParse "a -> a -> Bool"))
    , ("<", Builtin 2 compareLt (tParse "a -> a -> Bool"))
    , ("<=", Builtin 2 compareLte (tParse "a -> a -> Bool"))
    , (">", Builtin 2 compareGt (tParse "a -> a -> Bool"))
    , (">=", Builtin 2 compareGte (tParse "a -> a -> Bool"))
    , ("fst", Builtin 1 fst' (tParse "(a, b) -> a"))
    , ("snd", Builtin 1 snd' (tParse "(a, b) -> b"))
    --, ("fix", Builtin 1 fixOp (tParse "(a -> a) -> a"))
    , ("length", Builtin 1 listLength (tParse "[a] -> Int"))
    , ("head", Builtin 1 listHead (tParse "[a] -> a"))
    , ("tail", Builtin 1 listTail (tParse "[a] -> [a]"))
    , ("cons", Builtin 2 listCons (tParse "a -> [a] -> [a]"))
    , ("isEmpty", Builtin 1 listIsEmpty (tParse "[a] -> Bool"))
    , ("println", Builtin 1 (printStr True) (tParse "Str -> ()"))
    , ("print", Builtin 1 (printStr False) (tParse "Str -> ()"))
    , ("printVarLn", Builtin 1 (printVar True) (tParse "a -> ()"))
    , ("printVar", Builtin 1 (printVar False) (tParse "a -> ()"))
    , ("getLine", Builtin 0 (readFrom stdin) (tParse "Str"))
    , ("show", Builtin 1 showVar (tParse "a -> Str"))
    , ("readInt", Builtin 1 readInt (tParse "Str -> Int"))
    , ("readFloat", Builtin 1 readFloat (tParse "Str -> Float"))
    , ("readBool", Builtin 1 readBool (tParse "Str -> Bool"))

--    , ("seq", Builtin 2 liplSeq (tParse "a -> b -> b"))
--    , ("env", Builtin 1 showEnvironment)
--    , ("free-vars", Builtin 1 getFreeVars)
--    , ("from-str", Builtin 1 strToList)
--    , ("from-list", Builtin 1 listToStr)
    ]

primitivesList = Map.toList primitives

builtinNames = map fst primitivesList
builtins = map snd primitivesList

builtinSubst = map mkSubst primitivesList
    where
        mkSubst (a, b) = (a, getBuiltinType b)

fst' [(Pair x _)] = return x
snd' [(Pair _ x)] = return x

readInt [(Str s)] = return (Int (read s))
readInt [(List l)] = readInt [toStr l]
readFloat [(Str s)] = return (Float (read s))
readFloat [(List l)] = readFloat [toStr l]
readBool [(Str s)] = return (Bool (read s))
readBool [(List l)] = readBool [toStr l]

toInt [(Float f)] = return (Int $ floor f)
toFloat [(Int i)] = return (Float $ fromIntegral i)

--fixOp [Fun ] = fix f

compareEq :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareEq = compareOp (ordBool [EQ])

compareNeq :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareNeq = compareOp (ordBool [LT, GT])

compareLt :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareLt = compareOp (ordBool [LT])

compareLte :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareLte = compareOp (ordBool [LT, EQ])

compareGt :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareGt = compareOp (ordBool [GT])

compareGte :: (MonadEval m, E.MonadError Err m, MonadPos m) => [Val] -> m Val
compareGte = compareOp (ordBool [GT, EQ])

ordBool target x = elem x target

--compareOp op [a, b] =
--    return $ Bool $ op (unpackVal a `compare` unpackVal b)
compareOp :: (MonadEval m, E.MonadError Err m, MonadPos m)
    => (Ordering -> Bool) -> [Val] -> m Val
compareOp op [Int a, Int b] = return $ Bool $ op (compare a b)
compareOp op [Float a, Int b] =
    return $ Bool $ op (compare a (fromIntegral b))
compareOp op [Int a, Float b] =
    return $ Bool $ op (compare (fromIntegral a) b)
compareOp op [Float a, Float b] = return $ Bool $ op (compare a b)
compareOp op [Bool a, Bool b] = return $ Bool $ op (compare a b)
compareOp op [Str a, Str b] = return $ Bool $ op (compare a b)
compareOp op [a@(Str _), List l] = compareOp op [a, toStr l]
compareOp op [List l, a@(Str _)] = compareOp op [toStr l, a]
compareOp op [Char a, Char b] = return $ Bool $ op (compare a b)
compareOp op [List a, List b] = return $ Bool $ op (compare a b)
compareOp op e@([a,b]) = do
    pos <- getSourcePos
    E.throwError $ Err pos ("Can't compare: " ++ show e)

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
--opAdd [Int a, Float b] = return $ Float (fromIntegral a + b)
--opAdd [Float a, Int b] = return $ Float (a + fromIntegral b)
--opAdd x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
--opAdd x = E.throwError $ ArityErr 2 x

opSub [Int a, Int b] = return $ Int (a - b)
opSub [Float a, Float b] = return $ Float (a - b)
--opSub [Int a, Float b] = return $ Float (fromIntegral a - b)
--opSub [Float a, Int b] = return $ Float (a - fromIntegral b)
--opSub x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
--opSub x = E.throwError $ ArityErr 2 x

opMult [Int a, Int b] = return $ Int (a * b)
opMult [Float a, Float b] = return $ Float (a * b)
--opMult [Int a, Float b] = return $ Float (fromIntegral a * b)
--opMult [Float a, Int b] = return $ Float (a * fromIntegral b)
--opMult x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
--opMult x = E.throwError $ ArityErr 2 x

--floatOpDiv x@([Int a, Int b]) =
--    E.throwError $ TypeErr "2 Floats" (Expr x)
floatOpDiv [Float a, Float b] = return $ Float (a / b)
--floatOpDiv [Int a, Float b] = return $ Float (fromIntegral a / b)
--floatOpDiv [Float a, Int b] = return $ Float (a / fromIntegral b)
--floatOpDiv x@([_, _]) = E.throwError $ TypeErr "Float" (Expr x)
--floatOpDiv x = E.throwError $ ArityErr 2 x

intOpDiv [Int a, Int b] = return $ Int (div a b)
--intOpDiv [Float a, Int b] = return $ Int (div (floor a) b)
--intOpDiv [Int a, Float b] = return $ Int (div a (floor b))
--intOpDiv x@([Float a, Float b]) =
--    E.throwError $ TypeErr "2 Ints" (Expr x)
--intOpDiv x@([_, _]) = E.throwError $ TypeErr "Int" (Expr x)
--intOpDiv x = E.throwError $ ArityErr 2 x

boolBinOp op [Bool a, Bool b] = return $ Bool (a `op` b)
--boolBinOp op x@([Bool _, _]) =
--    E.throwError $ TypeErr "Bool for 2nd arg" (Expr x)
--boolBinOp op x@([_, Bool _]) =
--    E.throwError $ TypeErr "Bool for 1st arg" (Expr x)
--boolBinOp op x = E.throwError $ ArityErr 2 x

boolNot [Bool a] = return $ Bool (not a)
--boolNot [x] = E.throwError $ TypeErr "Bool" x
--boolNot x = E.throwError $ ArityErr 1 x

listLength [List x] = return $ Int (toInteger $ length x)
listLength [Str x] = return $ Int (toInteger $ length x)
--listLength [x] = E.throwError $ TypeErr "need list" x
--listLength x = E.throwError $ ArityErr 1 x

listHead [List (x:xs)] = return x
listHead [e@(List [])] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty list: " ++ show e)
listHead [Str (x:xs)] = return $ Char x
listHead [e@(Str "")] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty string: " ++ show e)
listHead [x] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty list: " ++ show x)
--listHead x = E.throwError $ ArityErr 1 x

listTail [List (x:xs)] = return $ List xs
listTail [e@(List [])] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty list: " ++ show e)
listTail [Str (x:xs)] = return $ Str xs
listTail [e@(Str [])] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty string: " ++ show e)
listTail [x] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need non empty list: " ++ show x)
--listTail x = E.throwError $ ArityErr 1 x

listCons [x, List []] = return $ List [x]
listCons [x, List xs] = return $ List (x:xs)
listCons [Char x, Str ""] = return $ Str [x]
listCons [Char x, Str xs] = return $ Str (x:xs)
--listCons e@([_,_]) = E.throwError $ TypeErr "need list" (List e)
--listCons x = E.throwError $ ArityErr 2 x

listIsEmpty [List a] = return $ Bool (null a)
listIsEmpty [Str a] = return $ Bool (null a)
--listIsEmpty [x] = E.throwError $ TypeErr "need list" x
--listIsEmpty x = E.throwError $ ArityErr 1 x

printStr newLine [Str x] = do
    T.liftIO $ putStr  x
    T.liftIO $ putChar (if newLine then '\n' else '\0')
    T.liftIO $ hFlush stdout
    return Null
--printStr _ [x] = E.throwError $ TypeErr "need string" x
--printStr _ x = E.throwError $ ArityErr 1 x

printVar newLine [Str x] = do
    T.liftIO $ putStr x
    T.liftIO $ putChar (if newLine then '\n' else ' ')
    T.liftIO $ hFlush stdout
    return Null
printVar newLine [x] = printVar newLine [Str $ show x]
--printVar _ x = E.throwError $ ArityErr 1 x

showVar [x] = return $ Str (show x)
--showVar x = E.throwError $ ArityErr 1 x

showEnvironment [Str key] = do
    env <- getEnv
    if key == ""
        then
            return $ Dict (Map.toList env)
        else
            do
                --val <- Map.lookup key env
                val <- getVal key
                return val

strToList [Str s] = return (List $ map Char s)
strToList [x] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need string: " ++ show x)
strToList x = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need 1 arg: " ++ show x)

listToStr [List l] = do
    s <- mapM toChar l
    return $ Str s
    where
        toChar (Char c) = return c
        toChar x = do
            pos <- getSourcePos
            E.throwError $ Err pos ("need list of chars: " ++ show x)
listToStr [x] = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need list" ++ show x)
listToStr x = do
    pos <- getSourcePos
    E.throwError $ Err pos ("need 1 arg: " ++ show x)

--getFreeVars [x] = return $ List (map Ident (unboundVars x))

readFrom handle [] = do
    s <- T.liftIO $ hGetLine handle
    return $ Str s
