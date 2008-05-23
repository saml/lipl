==================
EvalMonadClass.lhs
==================

EvalMonadClass declares MonadEval class that provides
actions useful for evaluation of LIPL expressions.

.. sc:: lhs

> module EvalMonadClass where
>
> import LangData
>
> class (Monad m) => MonadEval m where
>     getEnv :: m Env
>     getEnvs :: m EnvStack
>     putEnvs :: EnvStack -> m ()
>     pushEnv :: Env -> m ()
>     popEnv :: m ()

- getEnv returns an Env (Map String Val).
- getEnvs returns a Stack of Env's (``[Env]``).
- putEnvs replaces the Stack of Env's.
- pushEnv pushes an Env on top of the Stack of Env's.
- popEnv pops the top Env from the Stack of Env's.

So, anything that is an instance of MonadEval
would have a global Stack of Env's.


