module Stack where

type Stack a = [a]
type Queue a = [a]

isEmpty :: Stack a -> Bool
isEmpty = null

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

push :: a -> Stack a -> Stack a
push v s = v : s

front :: Queue a -> (a, Queue a)
front (x:xs) = (x, xs)
