module Stack where

data Stack a = St [a]
    deriving (Eq, Ord, Show)

push :: a -> (Stack a) -> (Stack a)
push x (St xs) = St (x:xs)

pop :: (Stack a) -> (a, Stack a)
pop (St (x:xs)) = (x, (St xs))

isempty :: (Eq a) => (Stack a) -> Bool
isempty (St x) = (x == [])

emptystack :: Stack a
emptystack = St []
