===========
CoreLib.lhs
===========

.. sectnum::
.. contents::

CoreLib defines built-in functions for LIPL.

.. sc:: lhs

> {-# LANGUAGE Rank2Types, FlexibleContexts #-}
> --{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

Rank2Types extension is used to store
various ``[Val] -> m [Val]`` (built-in) functions
where m can be an instance of
MonadEval and/or MonadError Err and/or
MonadIO and/or MonadPos.

.. sc:: lhs

> module CoreLib where
>
> import qualified Control.Monad as M
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.Trans as T
> import qualified Data.Map as Map
> import System.IO
>
> import LangData
> import TParse
> import Type
> import EvalMonad
> import Error
> import PosMonad
>
> data Builtin = Builtin {
>     getBuiltinArity :: Int
>     , getBuiltinFun ::
>         forall m. (MonadEval m, E.MonadError Err m
>             , T.MonadIO m, MonadPos m) => [Val] -> m Val
>     , getBuiltinType :: Type
>     }

Builtin can store a built-in function.
A built-in function should have:

- number of arguments it takes.
- actual function that takes ``[Val]`` (arguments) and returns result.
- type of the function.

There can be a better way to store
built-in functions (getBuiltinFun) in Builtin than to use Rank2Type.

.. sc:: lhs

> funcall fname args = case Map.lookup fname primitives of
>     Nothing -> do
>         pos <- getSourcePos
>         E.throwError
>             $ Err pos ("Unrecognizable primitive function: " ++ fname)
>     Just f -> (getBuiltinFun f) args

Given a function name and list of Vals (arguments),
funcall calls the built-in function of the same name with the arguments.

.. sc:: lhs

> arityOf name = case Map.lookup name primitives of
>     Nothing -> (-1)
>     Just f -> getBuiltinArity f

Given name of a built-in function, arityOf returns
arity of the function.

.. sc:: lhs

> primitives = Map.fromList [
>     ("+", Builtin 2 opAdd (tParse "Int -> Int -> Int"))
>     , ("+.", Builtin 2 opAdd (tParse "Float -> Float -> Float"))
>     , ("-", Builtin 2 opSub (tParse "Int -> Int -> Int"))
>     , ("-.", Builtin 2 opSub (tParse "Float -> Float -> Float"))
>     , ("*", Builtin 2 opMult (tParse "Int -> Int -> Int"))
>     , ("*.", Builtin 2 opMult (tParse "Float -> Float -> Float"))
>     , ("div", Builtin 2 intOpDiv (tParse "Int -> Int -> Int"))
>     , ("/", Builtin 2 floatOpDiv (tParse "Float -> Float -> Float"))
>     , ("toInt", Builtin 1 toInt (tParse "Float -> Int"))
>     , ("toFloat", Builtin 1 toFloat (tParse "Int -> Float"))
>     , ("&&", Builtin 2 (boolBinOp (&&)) (tParse "Bool -> Bool -> Bool"))
>     , ("||", Builtin 2 (boolBinOp (||)) (tParse "Bool -> Bool -> Bool"))
>     , ("not", Builtin 1 boolNot (tParse "Bool -> Bool"))
>     , ("==", Builtin 2 compareEq (tParse "a -> a -> Bool"))
>     , ("!=", Builtin 2 compareNeq (tParse "a -> a -> Bool"))
>     , ("<", Builtin 2 compareLt (tParse "a -> a -> Bool"))
>     , ("<=", Builtin 2 compareLte (tParse "a -> a -> Bool"))
>     , (">", Builtin 2 compareGt (tParse "a -> a -> Bool"))
>     , (">=", Builtin 2 compareGte (tParse "a -> a -> Bool"))
>     , ("fst", Builtin 1 fst' (tParse "(a, b) -> a"))
>     , ("snd", Builtin 1 snd' (tParse "(a, b) -> b"))
>     , ("length", Builtin 1 listLength (tParse "[a] -> Int"))
>     , ("head", Builtin 1 listHead (tParse "[a] -> a"))
>     , ("tail", Builtin 1 listTail (tParse "[a] -> [a]"))
>     , ("cons", Builtin 2 listCons (tParse "a -> [a] -> [a]"))
>     , ("isEmpty", Builtin 1 listIsEmpty (tParse "[a] -> Bool"))
>     , ("println", Builtin 1 (printStr putStrLn) (tParse "Str -> ()"))
>     , ("print", Builtin 1 (printStr putStr) (tParse "Str -> ()"))
>     , ("getLine", Builtin 0 (readFrom stdin) (tParse "Str"))
>     , ("show", Builtin 1 showVar (tParse "a -> Str"))
>     , ("readInt", Builtin 1 readInt (tParse "Str -> Int"))
>     , ("readFloat", Builtin 1 readFloat (tParse "Str -> Float"))
>     , ("readBool", Builtin 1 readBool (tParse "Str -> Bool"))
>     ]
>
> primitivesList = Map.toList primitives
>
> builtinNames = map fst primitivesList
>
> builtinSubst = map mkSubst primitivesList
>     where
>         mkSubst (a, b) = (a, getBuiltinType b)

- primitives is a Map of String and built-in functions.
- builtinNames is a list of names of built-in functions.
- builtinSubst stores types (actually TScheme) of built-in functions.

.. sc:: lhs

> fst' [(Pair x _)] = return x
> snd' [(Pair _ x)] = return x

fst' and snd' returns fst or snd of given LIPL pair.

.. sc:: lhs

> readInt [(Str s)] = return (Int (read s))
> readInt [(List l)] = readInt [toStr l]
> readFloat [(Str s)] = return (Float (read s))
> readFloat [(List l)] = readFloat [toStr l]
> readBool [(Str s)] = return (Bool (read s))
> readBool [(List l)] = readBool [toStr l]

``read*`` converts LIPL string or list of chars to actual value
they represent.

.. sc:: lhs

> toInt [(Float f)] = return (Int $ floor f)
> toFloat [(Int i)] = return (Float $ fromIntegral i)

toInt and toFloat converts between Float and Int.

.. sc:: lhs

> isIn = flip elem

isIn takes a list and a value and tests if the value can be found
in the list::

    isIn [LT, EQ] EQ
    ==> True

.. sc:: lhs

> compareOp :: (E.MonadError Err m, MonadPos m) =>
>     (Ordering -> Bool) -> [Val] -> m Val
> compareOp op [Int a, Int b] = return $ Bool $ op (compare a b)
> compareOp op [Float a, Float b] = return $ Bool $ op (compare a b)
> compareOp op [Bool a, Bool b] = return $ Bool $ op (compare a b)
> compareOp op [Str a, Str b] = return $ Bool $ op (compare a b)
> compareOp op [a@(Str _), List l] = compareOp op [a, toStr l]
> compareOp op [List l, a@(Str _)] = compareOp op [toStr l, a]
> compareOp op [Char a, Char b] = return $ Bool $ op (compare a b)
> compareOp op [List a, List b] = return $ Bool $ op (compare a b)
> compareOp op [Pair a1 a2, Pair b1 b2] = do
>     ab1 <- compareOp op [a1, b1]
>     ab2 <- compareOp op [a2, b2]
>     return $ Bool (unpackBool ab1 && unpackBool ab2)
> compareOp _ [a,b] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("can't compare these two: "
>         ++ show a ++ ", " ++ show b)

compareOp compares two LIPL values.
To compare equality, for example,
``compareOp (isIn [EQ]) (compare a b)`` can be used.
compare a b will return EQ, LT, or GT.
When it returns EQ, ``isIn [EQ] EQ`` will return True.
When it returns something other than EQ, ``isIn [EQ] returnValue``
would be False.

.. sc:: lhs

> --compareEq :: (Monad m) => [Val] -> m Val
> compareEq :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareEq = compareOp (isIn [EQ])
>
> --compareNeq :: (Monad m) => [Val] -> m Val
> compareNeq :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareNeq = compareOp (isIn [LT, GT])
>
> --compareLt :: (Monad m) => [Val] -> m Val
> compareLt :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareLt = compareOp (isIn [LT])
>
> --compareLte :: (Monad m) => [Val] -> m Val
> compareLte :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareLte = compareOp (isIn [LT, EQ])
>
> --compareGt :: (Monad m) => [Val] -> m Val
> compareGt :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareGt = compareOp (isIn [GT])
>
> --compareGte :: (Monad m) => [Val] -> m Val
> compareGte :: (MonadPos m, E.MonadError Err m) => [Val] -> m Val
> compareGte = compareOp (isIn [GT, EQ])

``compare*`` functions above tests whether
two LIPL values are equal, less than, less than or equal to,
greater than, greater than or equal to.

.. sc:: lhs

> opAdd [Int a, Int b] = return $ Int (a + b)
> opAdd [Float a, Float b] = return $ Float (a + b)
>
> opSub [Int a, Int b] = return $ Int (a - b)
> opSub [Float a, Float b] = return $ Float (a - b)
>
> opMult [Int a, Int b] = return $ Int (a * b)
> opMult [Float a, Float b] = return $ Float (a * b)
>
> floatOpDiv [Float a, Float b] = return $ Float (a / b)
>
> intOpDiv [Int a, Int b] = return $ Int (div a b)

Above functions performs arithmetic operations on LIPL values.

.. sc:: lhs

> boolBinOp op [Bool a, Bool b] = return $ Bool (a `op` b)
>
> boolNot [Bool a] = return $ Bool (not a)

boolBinOp performs boolean binary operation on LIPL values
according to op (||, &&, ...).
boolNot flips True to False and vice versa.

.. sc:: lhs

> listLength [List x] = return $ Int (toInteger $ length x)
> listLength [Str x] = return $ Int (toInteger $ length x)
>
> listHead [List (x:xs)] = return x
> listHead [e@(List [])] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty list: " ++ show e)
> listHead [Str (x:xs)] = return $ Char x
> listHead [e@(Str "")] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty string: " ++ show e)
> listHead [x] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty list: " ++ show x)
>
> listTail [List (x:xs)] = return $ List xs
> listTail [e@(List [])] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty list: " ++ show e)
> listTail [Str (x:xs)] = return $ Str xs
> listTail [e@(Str [])] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty string: " ++ show e)
> listTail [x] = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("need non empty list: " ++ show x)
>
> listCons [x, List []] = return $ List [x]
> listCons [x, List xs] = return $ List (x:xs)
> listCons [Char x, Str ""] = return $ Str [x]
> listCons [Char x, Str xs] = return $ Str (x:xs)
>
> listIsEmpty [List a] = return $ Bool (null a)
> listIsEmpty [Str a] = return $ Bool (null a)

Above are built-in list operations.
LIPL Str values are considered to be list of Chars.
So, Str cases are also implemented.

.. sc:: lhs

> showVar [x] = return $ Str (show x)

showVar is used to convert LIPL values to string representation.

.. sc:: lhs

> printStr f [Str x] = do
>     T.liftIO $ f x
>     T.liftIO $ hFlush stdout
>     return Null
> printStr f [List x] = printStr f [toStr x]
>
> readFrom handle [] = do
>     s <- T.liftIO $ hGetLine handle
>     return $ Str s

printStr prints Str. f is the IO action that prints.
f can be putStrLn, putStr, ...etc.
readFrom reads a line from handle (could be stdin).
