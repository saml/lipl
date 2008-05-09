{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}


module TIMonad where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad.Error as E

import Type
import CoreLib (builtinSubst)

type ErrMsg = String

{-
class (Monad m) => St s m | m -> s where
    getSubst :: m s
-}

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

newtype TInfer a = TInfer {
    runTInfer :: E.ErrorT ErrMsg TI a }
    deriving (E.MonadError ErrMsg, Monad, Functor)

{- data TI a = TI (Subst -> Int -> (Subst, Int, a))
    | Fail String
-}

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> case g x of
            TI gx -> gx s' n'
            {- Fail msg -> (s', n', x) -})
    --Fail msg >>= _ = E.throwError msg
    --fail = Fail

{-
instance E.MonadError String TI where
    throwError = Fail
    ok@(TI _) `catchError` _ = ok
    Fail err `catchError` h = h err
-}

showTI ti = let (s,_,t) = runTI ti initialSubst 0
    in
        showSubstTypePair (s,t)

--runTI (TI f) s i = f s i
-- runTI (Fail msg) s i = Fail msg

{-
instance S.MonadState (TI a) a where
    get = TI (\s n -> (s, n, (s,n)))
    put (s,n) = TI (\_ _ -> (s, n, ()))
-}

getSubst = TI (\s n -> (s, n, s))
getN = TI (\s n -> (s, n, n))
putSubst s = TI (\_ n -> (s, n, ()))
putN n = TI (\s _ -> (s, n, ()))
extendSubst s' = TI (\s n -> (s @@ s', n, ()))
-- showSubst ([("x", TScheme [] tInt)] @@ [("x", TScheme [] tChar)])
-- runTI (extendSubst [("x", TScheme [] tChar)]) [("x", TScheme [] tInt)] 0
{-
getSubst = do
    s <- getSubst'
    return s
-}
unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u

newId = TI (\s n -> (s, n+1, "t" ++ show n))

newTVar = do
    v <- newId
    return (TVar v)

{-
find x assumptions = case lookup x assumptions of
    Just t -> return t
    otherwise -> fail $ "unbound type variable: " ++ x

unifiable t1 t2 = case runTI action initialSubst 0 of
    (_,_,result) -> result
    where
        action = (do
            unify t1 t2
            s <- getSubst
            return (apply s t1 == apply s t2))
-}




defaultSubst :: Subst
defaultSubst = toSubst [
    ("Int", tInt)
    , ("Float", tFloat)
    , ("Bool", tBool)
    , ("Char", tChar)
    , ("Str", list tChar)
    ]


fromIdType = map (\(k,v) -> (k, mkPolyType v))
toSubst = Map.fromList . fromIdType

initialSubst = defaultSubst `Map.union` toSubst builtinSubst
