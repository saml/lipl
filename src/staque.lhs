[Accompanying presentation](http://docs.google.com/Present?docid=ddr69psk_183cncbcz29&skipauth=true)

I was thinking about how to let users mix up prefix, infix, and postfix
syntax.

For example, `+ (+ 1 2) 3`, `1 + 2 + 3`, and `1 2 + 3 +`
would all be grammatical.

Using a stack and a queue, it might be possible:

- Each token (value) is pushed on to the stack.
- If the value is a function, pop from the stack to get arguments.
- If stack doesn't have enough arguments, pop from the queue.

Basically, it is stack based evaluation (postfix) with look ahead
to give users illusion of prefix or infix.

`1 + 2 + 3`, for example, would be evaluated as follows:

    Queue: 1 + 2 + 3
    Stack:
    --
    Queue: + 2 + 3
    Stack: 1
    --
    Queue: + 3
    Stack: 3
    --
    Queue:
    Stack: 6

I name the language **Staque** and here is prototype:

> module Main where

Import stuff for parser.

> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec ( (<|>) )

Import stuff for printing.

> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )

Import stuff for repl.

> import System.IO ( stdout, hFlush )

Let's define values for the language.

> data Val = Int Integer
>     | Ident String
>     | Expr [Val]

Let `Val` to be showable.

> ppVal (Ident s) = PP.text s
> ppVal (Int i) = PP.integer i
> ppVal (Expr xs) = PP.parens (PP.hsep $ map ppVal xs)
> instance Show Val where show = PP.render . ppVal

Let's write a parser.
Expression is just a list of tokens separated by whitespaces.

> parseExpr = do
>     toks <- P.sepEndBy parseToken ws
>     return $ Expr toks
> ws = P.skipMany1 P.space

Expression can be parenthesized.

> parseParenExpr = do
>     lparen
>     e <- P.sepEndBy parseToken ws
>     rparen
>     return $ Expr e
>     where
>         lparen = P.char '(' >> P.spaces
>         rparen = P.spaces >> P.char ')'

A token can be an integer literal, an identifier,
or a parenthesized expression.

> parseToken = do
>     P.try parseParenExpr
>     <|> P.try parseInt
>     <|> P.try parseIdent

Let's parse integer literal.
An integer literal can start with `-`.

> parseInt = do
>     sign <- P.string "-" <|> return ""
>     val <- nat
>     return $ Int (read $ sign ++ val)
> nat = P.many1 P.digit

Let's parse identifier.
Identifier can be operator or name.

> parseIdent = do
>     ident <- parseOp <|> parseName
>     return $ Ident ident
>     where
>         parseOp = parseHeadBody opChar opChar
>         parseName = parseHeadBody nameChar nameChar
>         parseHeadBody hChar bChar = do
>             h <- hChar
>             b <- P.many bChar
>             return (h : b)
>         opChar = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
>         nameChar = P.alphaNum <|> P.oneOf "_-'"

Now, onto actual evaluation.

Let's define stack and implement push and pop:

> type Stack = [Val]
> push v s = v : s
> pop (x:xs) = (x, xs)

Here is queue. We only consume a queue. Never push element to the queue.

> type Queue = [Val]
> front (x:xs) = (x, xs)

Evaluation function. Finally!

> eval :: Stack -> Queue -> Val

When the queue is empty, evaluation is done.
Make sure the stack has only 1 element and return the element as
the result of evaluation.

> eval s [] | length s == 1 = (fst . pop) s

Now, the queue's front is an identifier. Let's look it up and call
the function bound to the identifier.
Also, we make sure the rest of the queue is evaluated with
the updated stack.

> eval s (Ident fname : args) = let
>     (s', q) = funcall fname s args
>     in
>         eval s' q


The queue's front is not an identifier. Assume it's a literal and
push it to the stack and evaluate the rest of the queue.

> eval s (x : xs) = let
>     s' = push x s
>     in
>         eval s' xs

Funcall just looks up a function. If found, it calls the function with
stack and queue. The called function returns updated `(Stack, Queue)`.

> funcall fname s q = case lookup fname primitives of
>     Nothing -> error $ fname ++ " not defined"
>     Just f -> f s q

Funcall looks up this map.

> primitives = [
>     ("+", binNumOp (+))
>     , ("-", binNumOp (-))
>     , ("/", binNumOp div)
>     , ("*", binNumOp (*))
>     ]

Before we define a function that uses stack and queue to evaluate
binary numeric operations, let's define helper functions.

To unpack and pack integers from and to Val.

> fromVal (Int a) = a
> toVal a = Int a

To evaluate values popped from stack or queue.
If the popped value is an expression, evaluate the expression
using a new stack. Otherwise, just return the popped value.

> evalVal val = case val of
>     Expr q -> eval [] q
>     otherwise -> val

Now, onto evaluation of binary numeric operation.

First, stack has 2 elements.
So, both arguments to the binary operation can be popped from
the stack. The arguments popped are evaluated because they
can be nested expressions.
Then push the result of operation to the stack and return it with queue.

> binNumOp op s q | length s >= 2 = let
>     (b, s') = pop s
>     (a, s'') = pop s'
>     a' = evalVal a
>     b' = evalVal b
>     result = toVal $ fromVal a' `op` fromVal b'
>     in
>         (push result s'', q)

When stack has only 1 element, we should pop from the queue, too.

> binNumOp op s q | length s >= 1 = let
>     (a, s') = pop s
>     (b, q') = front q
>     a' = evalVal a
>     b' = evalVal b
>     result = toVal $ fromVal a' `op` fromVal b'
>     in
>         (push result s', q')

Stack is empty. So, pop 2 arguments from the queue.

> binNumOp op s q = let
>     (a, q') = front q
>     (b, q'') = front q'
>     a' = evalVal a
>     b' = evalVal b
>     result = toVal $ fromVal a' `op` fromVal b'
>     in
>         (push result s, q'')

Now, let's make a repl.

> repl = do
>     input <- prompt "staque> "
>     if input == ":q"
>         then putStrLn "bye"
>         else do
>             putStrLn $ evaluate input
>             repl
>     where
>         prompt p = do
>             putStr p
>             hFlush stdout
>             getLine

Actual evaluate function that transforms user input to string.

> evaluate s = case P.parse parseExpr "staque" s of
>     Left err -> show err
>     Right (Expr q) -> show $ eval [] q

Finally, main function.

> main = repl

Let's run it!

    $ runhaskell staque.lhs
    staque> 1 + 2
    3
    staque> + 1 2
    3
    staque> 1 2 +
    3
    staque> 1 + 2 + ((1 - -2) * 3) 3 / (+ 1 2) *
    12
    staque> :q
    bye

For exercises:

- Add more types like string, float, list, char, bool...
  to the language.
- Add function definition and lambda abstraction.
- Add static type checking with user defined types.
- Get fancier with type system.
- Add foreign function interface.
- Add OpenGL binding.
- Make a first point shooter.
