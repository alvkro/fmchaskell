{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , String(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show :: Nat -> String
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    (==) O O = True -- primeiro caso
    (==) (S x) (S y) = x == y -- faz a captura recursiva
    (==) _ _ = False -- ultimo dos casos


instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    (<=) (S _) O = False
    (<=) O _ = True
    (<=) (S x) (S y) = x <= y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min x _ = x
    min y _ = y

    max = undefined


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = False
isZero (S O) = True

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) x O = x
(<+>) (S x) y = S (x <+> y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus m O = m
monus (S m) (S n) = m `monus` n
monus _ _ = O

(-*) :: Nat -> Nat -> Nat
(-*) O O = O
(-*) _ O = O
(-*) O _ = O

-- multiplication
times :: Nat -> Nat -> Nat
times O O = O
times _ O = O
times m (S n) = m * n <+> m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = undefined

exp :: Nat -> Nat -> Nat
exp = undefined

(<^>) :: Nat -> Nat -> Nat
(<^>) = undefined

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = undefined

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = undefined

(|-|) = dist

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg m = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O a = undefined
lo (S O) a = undefined  
lo b O = undefined
lo b a = 
    if (>=) a b == O 
    then O 
    else S (lo b (a / b))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

