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
    min y O = y
    min O x = x
    min (S x) (S y) = S (x `min` y)

    max :: Nat -> Nat -> Nat
    max O x = x
    max y O = y
    max (S x) (S y) = S (x `max` y)


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
(-*) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times O O = O
times _ O = O
times m (S n) = m `times` n <+> m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow m O = one
pow m (S n) = m `pow` n `times` m

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient (quantas vezes x cabe em y?)
(</>) :: Nat -> Nat -> Nat
(</>) m O = undefined
(</>) m n =
    case monus m n of -- Estrutura Switch-case (C++)
        O -> if m == n then one else O
        diff -> S (diff </> n) -- diff seria semelhante ao caso "default" de um switch case

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) m O = undefined
(<%>) m n = 
    case monus m n of   
        O -> if m == n then O else m  
        diff -> diff <%> n  

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (m, n) =
    if m >= n
    then let (q, r) = eucdiv (m - n, n)
         in (S q, r) -- +1 no quociente
    else (O, m)  

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) m n = if m % n == O -- é redundante colocar then/else

divides :: Nat -> Nat -> Bool -- ???
divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist m O = m
dist O n = n
dist (S m) (S n) = m `dist` n

(|-|) :: Nat -> Nat -> Nat
(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S m) = factorial m * S m

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
toNat n
    | n == 0    = O
    | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = error "Não existe número natural negativo"
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

