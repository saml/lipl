==================
EvalMonadClass.lhs
==================

.. sectnum::
.. contents::

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
- getEnvs returns a Stack of Envs (``[Env]``).
- putEnvs replaces the Stack of Envs.
- pushEnv pushes an Env on top of the Stack of Envs.
- popEnv pops the top Env from the Stack of Envs.

So, anything that is an instance of MonadEval
would have a global Stack of Envs.


