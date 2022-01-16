class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x /= y = not (x Main.== y)
    x == y = not (x Main./= y)

class (Main.Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool

    x <= y = (x Main.< y) || (x Main.== y)
    x > y = not (x Main.<= y)
    x >= y = (x Main.> y) || (x Main.== y)

instance (Num b) => Main.Eq (a -> b) where
    _ == _ = False

instance (Num b) => Main.Ord (a -> b) where
    _ < _ = False

instance (Num b) => Num (a -> b) where
    (+) f g x = (f x) + (g x)
    (*) f g x = (f x) * (g x)
    (-) f g x = (f x) - (g x)
    abs f x = abs (f x)
    signum f x = signum (f x)
    negate f x = negate (f x)
    fromInteger n = always n
                           where always n y = (fromInteger n)
