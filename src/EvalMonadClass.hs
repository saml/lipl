module EvalMonadClass where

import LangData

class (Monad m) => MonadEval m where
    getEnv :: m Env
    getEnvs :: m EnvStack
    putEnvs :: EnvStack -> m ()
    pushEnv :: Env -> m ()
    popEnv :: m ()
