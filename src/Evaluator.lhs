=============
Evaluator.lhs
=============

.. sectnum::
.. contents::

Evaluator module defines eval function that evaluates LIPL values.

.. sc:: lhs

> module Evaluator where
>
> import qualified Control.Monad.Error as E
>
> import qualified Data.Map as Map
>
> import LangData
> import CoreLib
> import LangUtils
> import Utils
> import EvalMonad
> import PosMonad
> import Error
>
> eval e@(Int _) = return e
> eval e@(Float _) = return e
> eval e@(Bool _) = return e
> eval e@(Char _) = return e
> eval e@(Str _) = return e

For literals above, just return itself.

.. sc:: lhs

> eval (At pos e) = do
>     setSourcePos pos
>     eval e

For At value that comes with SourcePos
and Val, store current SourcePos and evaluate the Val.

.. sc:: lhs

> eval e@(Ident var) = do
>     val <- getVal var
>     eval val

For an identifier, find its value from the EnvStack (using getVal).
When bound value is found, evaluate it.

.. sc:: lhs

> eval (List xs) = do
>     elems <- mapM eval xs
>     return $ List elems

For a list, evaluate each element in the list and return.

.. sc:: lhs

> eval (PrimFun name) = do
>     let arity = arityOf name
>     let fun = Prim name arity []
>     if arity == 0
>         then
>             eval fun
>         else
>             return fun

For built-in functions, return Prim function object
that stores remaining number of parameters to take and already taken
parameters.

.. sc:: lhs

> eval (Prim name 0 args) = funcall name args

For Prim function object that has no more parameters to take,
call the built-in function using funcall.

.. sc:: lhs

> eval (Lambda [] body) = eval body

For a lambda without parameter, just evaluate body.

.. sc:: lhs

> eval e@(Lambda args body) = do
>     env <- getEnvFor (unboundVars e)
>     let fun = Fun env args body
>     return fun

For a lambda expression, get free variables in e.
And get environment for the free variables.
For example, ``(lambda (x) (+ x y))`` has 1 free variable, y.
So, get environment for y (bound value for y).
Then, return a function object (closure) wrapped with the environment.

.. sc:: lhs

> eval e@(FunDef name args body) = do
>     env <- getEnvFor (unboundVars e)
>     putVal name (Fun env args body)
>     env' <- getEnv
>     let fun = Fun env' args body
>     updateVal name fun
>     return fun

For function definitions, make a function closure
that has environment for free variables in the function definition.
Then, update the environment with the closure
bound to the function name.
Then create another closure that has the environment
where the function name is bound to the previous closure.
Reset function name to the closure just created.
All this is to evaluate recursive functions.

.. sc:: lhs

> eval (If pred ifCase elseCase) = do
>     ifOrElse <- eval pred
>     case ifOrElse of
>         Bool False -> eval elseCase
>         otherwise -> eval ifCase

For if expression, evaluate the predicate.
If the predicate is True, evalute ifCase. Otherwise, evaluate elseCase.

.. sc:: lhs

> eval e@(Let env body) = do
>     env' <- keyValToEnv env
>     withEnv env' (eval body)

For let expression, evaluate key-val pairs and get
an environment where values are bound to the keys.
Then in that environment, evaluate body of let.

.. sc:: lhs

> eval (Fun env [] body) = do
>     withEnv env (eval body)

For function objects (closures) without parameters,
evaluate the body with environment the closure provides.

.. sc:: lhs

> eval (Expr []) = return Null
>
> eval (Expr [e]) = eval e -- unwrap outer parens

For empty expression, return Null.
For an expression with single value, evaluate the value.

.. sc:: lhs

> eval (Expr (f:e)) = do -- both f and e are Val because Expr [e] case
>     let firstArg = head e
>     let restArgs = tail e
>     function <- eval $ f
>     case function of
>         fun@(Fun env _ _) -> do
>             arg1 <- eval $ firstArg
>             evalFun env fun arg1 restArgs
>         fun@(Prim name remaining args) -> do
>             arg1 <- eval $ firstArg
>             evalPrim name arg1 restArgs args remaining
>         otherwise -> do
>             pos <- getSourcePos
>             E.throwError $ Err pos ("not function: " ++ show f)

For expressions that has at least two values,
evaluate the first value.
When it is a function (built-in or not), evaluate the second value
(1st argument to the function).
And curry (return a function object that has the argument
applied).
If the first value is not a function, throwError.

.. sc:: lhs

> eval (Pair e1 e2) = do
>     v1 <- eval e1
>     v2 <- eval e2
>     return $ Pair v1 v2

For a pair, evaluate each element of the pair and return the evaluated pair.

.. sc:: lhs

> eval x = return x

For all other cases, just return itself.

.. sc:: lhs

> evaluate (At _ e@(FunDef _ _ _)) = evaluate e
> evaluate e@(FunDef name args body) = do
>     result <- runLocally (eval e)
>     updateVal name result
>     return result
> evaluate (At _ (Expr [e])) = evaluate e
> evaluate (Expr [e]) = evaluate e
> evaluate e = runLocally (eval e)

evaluate runs eval in local environment so that the environment
is not cluttered with unnecessary key-val bindings.
Only function definitions modify the environment.

.. sc:: lhs

> runLocally action = do
>     prev <- getEnvs
>     pushEnv emptyEnv
>     result <- action
>     putEnvs prev
>     return result

runLocally saves current environment, runs the action, and restores
the environment.
emptyEnv is pushed to EnvStack so that the action can run on the new Env.

.. sc:: lhs

> keyValToEnv kv = runLocally (keyValToEnv' kv)
>     where
>         keyValToEnv' ((k,v):xs) = do
>             v' <- eval v
>             putVal k v'
>             keyValToEnv' xs
>         keyValToEnv' [] = do
>             env <- getEnv
>             return env

keyValToEnv takes key-value pair and evaluates
values one by one while updating environment with the evaluated value
bound to the key.

.. sc:: lhs

> apply (Fun env (arg:rst) body) e = do
>     let env' = Map.insert arg e env
>     if null rst
>         then
>             withEnv env'(eval body)
>         else
>             return $ Fun env' rst body
> apply e _ = do
>     pos <- getSourcePos
>     E.throwError $ Err pos ("not function: " ++ show e)

apply applies 1 argument to the given function object.
When no more argument is left to apply, body of the function object is called.

.. sc:: lhs

> evalFun env fun arg1 restArgs = do
>     withEnv env (do
>         partial <- apply fun arg1
>         eval $ Expr (partial : restArgs))

evalFun creates a partially applied function object by applying
arg1 to fun,
and evaluates the partially applied function object with the rest
of arguments.

.. sc:: lhs

> evalPrim fname arg1 restArgs argsHave remaining = do
>     let args = argsHave ++ [arg1]
>     if remaining == 1
>         then
>             funcall fname args
>         else
>             eval $ Expr (
>                 (Prim fname (remaining-1) args) : restArgs)

evalPrim calls built-in function when all of the arguments
are already available.
Otherwise, Prim function object (with 1 argument added to it)
is evaluated with the rest of arguments.

.. sc:: lhs

> withEnv env action = do
>     (do
>         pushEnv env
>         val <- action
>         popEnv
>         return val)
>     `E.catchError`
>     (\e -> do
>         popEnv
>         E.throwError e)

withEnv evaluates the given action in the environment provided.


