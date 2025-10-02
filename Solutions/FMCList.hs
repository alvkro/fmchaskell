{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Data.Binary.Builder (empty)

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "lista vazia!"
head (x:xs) = x -- Leia-se: pattern-maching em (x:xs) = x :)

tail :: [a] -> [a]
tail [] = error "lista vazia!"
tail (x:xs) = xs 

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: Integral i => [a] -> i
length [] = 0
length (x:xs) = 1 + length xs


sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1 -- Tem que ser 1 porque se fosse zero ia zerar tudo (se lembre de factorial!)
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = xs ++ [x] -- O Haskell interpreta head como apenas um caractere, não uma lista (safado)

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x:xs) = (x:xs) ++ [y]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Lista vazia!"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "Lista vazia!"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)


take :: Int -> [a] -> [a]
take 0 (x:xs) = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs


drop :: Int -> [a] -> [a]
drop 0 (x:xs) = []
drop _ [] = []
drop n (x:xs) = x : drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []                   

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = x:xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs
--              ^^^ LISTA INTEIRA

init :: [a] -> [[a]]
init [] = [[]]
init (x:xs) = (x:xs) : init xs


inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : [x:ys | ys <- inits xs]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs       -- se for true, x será incluido
    | otherwise = filter p xs  

map :: (a -> b) -> [a] -> [b]
map _ [] = []                       
map f (x:xs) = f x : map f xs -- Aplica uma function em x e retorna a lista pós função

cycle :: [a] -> [a]
cycle [] = error "Lista vazia!"
cycle xs = xs' where xs' = xs ++ xs'

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True                    
isPrefixOf _ [] = False                   
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = xs == drop (length ys - length xs) ys

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

transpose :: [[a]] -> [[a]]
transpose [] = []
-- ???

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome x = x == reverse x

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

