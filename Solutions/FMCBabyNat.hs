module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O -- Caso 1: Se meu imput for O
isZero x = O -- Caso 2: Se meu imput for qualquer outra coisa

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat -- Par
even O = S O
even (S n) = odd n

odd :: Nat -> Nat -- Impar
odd O = O
odd (S n) = even n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus x O = x
monus O y = O
monus (S x) (S y) = monus x y

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
(*) x  O = O
(*) x (S y) = x * y + x -- é como se pegasse o 3 e fosse decompondo em vários x (nesse caso 2) até que chegasse ao caso O, resultando em 6"
--               ^^^
--         SHADOWING DA VARIAVEL Y PARA Y - 1 ATÉ O CASO BASE
infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) x O = one
(^) x (S y) = x ^ y * x

infixr 8 ^

-- greater than or equal
(>=) :: Nat -> Nat -> Nat
(>=) O O = S O
(>=) O (S _) = O
(>=) (S _) O = S O  
(>=) (S n) (S m) = (>=) n m

-- less than or equal
(<=) :: Nat -> Nat -> Nat
(<=) O _ = one
(<=) _ O = zero
(<=) (S n) (S m) = (<=) n m

-- PS: Todos essas definições acima eu tive que tirar do Zullip (caso alguma hora eu precise...)

-- quotient
(/) :: Nat -> Nat -> Nat
(/) x O = undefined
(/) x y = -- Tentativa de fazer com Pattern Matching
    if monus x y == x -- a estrutura if-then-else assemelha ao operador ternário
    then if x == y then one else O
    else S (monus x y) / y

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
(%) x O = undefined
(%) x y = 
    if monus x y == y
    then x 
    else monus x y % y

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) x y = 


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) x y = 
    if x == y 
    then O 
    else if (>=) x y == S O 
         then monus x y 
         else monus y x

factorial :: Nat -> Nat
factorial O = S O
factorial (S x) = factorial x * S x

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg x = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O a = undefined
lo (S O) a = undefined  
lo b O = undefined
lo b a = 
    if (>=) a b == O 
    then O 
    else S (lo b (a / b))
-- A minha utilização de X e Y fica horrível nesse teorema T_T
