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
isZero m = O -- Caso 2: Se meu imput for qualquer outra coisa

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
monus m O = m
monus O n = O
monus (S m) (S n) = monus m n

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
(*) m  O = O
(*) m (S n) = m * n + m -- é como se pegasse o 3 e fosse decompondo em vários x (nesse caso 2) até que chegasse ao caso O, resultando em 6"
--               ^^^
--         SHADOWING DA VARIAVEL Y PARA Y - 1 ATÉ O CASO BASE
infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) m O = one
(^) m (S n) = m ^ n * m

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
(/) m O = undefined
(/) m n =
    case monus m n of -- Estrutura Switch-case (C++)
        O -> if m == n then one else O
        diff -> S (diff / n) -- diff seria semelhante ao caso "default" de um switch case

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
(%) m O = undefined
(%) m n = 
    case monus m n of   
        O -> if m == n then O else m  
        diff -> diff % n              

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) m n = if m % n == O then S O else O 

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff m n = m |-| n -- Mesma coisa que o |-|

(|-|) :: Nat -> Nat -> Nat
(|-|) m n = 
    if m == n 
    then O 
    else if (>=) m n == S O 
         then monus m n
         else monus n m

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
-- A minha utilização de X e Y fica horrível nesse teorema T_T

-- Isso TUDO é pattern matching:
-- f 0 = ...            Padrão de valor
-- f (S n) = ...        Padrão de construtor  
-- f (x:xs) = ...       Padrão de lista
-- f (a, b) = ...       Padrão de tupla
-- f _ = ...            Padrão coringa
