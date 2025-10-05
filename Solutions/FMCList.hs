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

-- (snoc is cons written backwards) [basicamente o oposto da função de um construtor :P]
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x:xs) = (x:xs) ++ [y] -- concatena a lista usando a função (++)

(<:) :: [a] -> a -> [a] -- Só troca a ordem dos argumentos :P
(<:) = flip snoc

-- different implementation of (++) [concatenação de listas de forma alternativa]
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
minimum (x:xs) = min x (minimum xs) -- compara o valor de x e aplica várias chamadas recursivas em xs!

maximum :: Ord a => [a] -> a
maximum [] = error "Lista vazia!"
maximum [x] = x
maximum (x:xs) = max x (maximum xs) -- mesmo processo do de cima...


take :: Int -> [a] -> [a] -- "quero n elementos dessa lista"
take 0 (x:xs) = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs -- pega o primeiro elemento e pega recursivamente até chegar em algum dos dois primeiros casos


drop :: Int -> [a] -> [a] -- oposto de take
drop 0 (x:xs) = x:xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs -- ignora o primeiro e continua (é praticamente a msm coisa do take^-1)

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

init :: [a] -> [a] -- ignora o ultimo elemento da lista
init [] = error "Lista vazia!"
init [x] = []  
init (x:xs) = x : init xs  


inits :: [a] -> [[a]] -- ignora todas as "heads" da lista
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs) -- mapeia todos os heads e ignora

subsequences :: [a] -> [[a]] -- retorna 2^n de conjuntos a partir do conjunto de entrada
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

any :: (a -> Bool) -> [a] -> Bool -- verifica se a lista satisfaz pelo menos uma condição dada no imput
any _ [] = False
any p (x:xs) = p x || any p xs -- elemento atual || verificação recursiva dos outros elementos

all :: (a -> Bool) -> [a] -> Bool -- fiscaliza se todos obedecem a uma certa regra
all _ [] = True
all p (x:xs) = p x && all p xs

and :: [Bool] -> Bool -- FMCBool.hs?
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool -- FMCBool.hs?
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss -- xs: primeira lista | xss: resto das listas

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

filter :: (a -> Bool) -> [a] -> [a] -- filtra (durr) com base em uma função (even, odd, >= 0).
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs  -- se for true, x será incluido
    | otherwise = filter p xs      -- se não, so ignora         

map :: (a -> b) -> [a] -> [b] -- cria uma nova lista ao aplicar uma função em cada elemento da lista
map _ [] = []                       
map f (x:xs) = f x : map f xs -- Aplica uma function em x e retorna a lista pós função

cycle :: [a] -> [a] -- lista infinita a partir da lista do imput, repetindo indefinidamente
cycle [] = error "Lista vazia!"
cycle xs = xs' where xs' = xs ++ xs' -- usar take?

repeat :: a -> [a] -- repete :P
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x -- preserva o primeiro elemento e aplica recursão até chegar ao caso zero

isPrefixOf :: Eq a => [a] -> [a] -> Bool -- verifica se há um pré-fixo da primeira lista na segunda
isPrefixOf [] _ = True                    
isPrefixOf _ [] = False                   
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys -- head = head AND tail = tail

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = any (isPrefixOf xs) (tails ys) -- gera todas as as caldas e verifica se elas são prefixas da segunda lista

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = xs == drop (length ys - length xs) ys -- Remove x elementos de ys e compara se é igual a xs

zip :: [a] -> [b] -> [(a,b)] -- Lista de pares
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys -- Pega os dois primeiros elementos (l1, l2) e recursivamente vão criando outros pares (tuplas) dentro de uma lista

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- Aplica funções nos pares
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys -- aplica f em x e y e continua nesse ciclo recursivamente até o caso base

intercalate :: [a] -> [[a]] -> [a] -- adiciona um separador entre duas listas
intercalate _ [] = []
intercalate y (x:[]) = x
intercalate y (x:xs) = x ++ y ++ intercalate y xs -- y é o caractere que separa as duas listas

nub :: Eq a => [a] -> [a] -- Remove elementos duplicados
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)  -- remove ocorrencias de x

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- RESPOSTA: O algoritmo percorre a lista duas vezes
-- uma vez no take, outra no drop, é ineficiente comparado
-- ao fazer com recursão...

break :: (a -> Bool) -> [a] -> ([a], [a]) -- Para conforme a condição imposta
break _ [] = ([], [])   
break p (x:xs)
    | p x       = ([], x:xs) -- < Para aqui
    | otherwise = (x:ys, zs) -- < Continua
        where (ys, zs) = break p xs

lines :: String -> [String] -- Divide uma string em linhas após o caractere especial \n
lines [] = []
lines x = takeWhile (/= '\n') x : lines (drop 1 (dropWhile (/= '\n') x))


words :: String -> [String] -- Divide uma string em palavras
words [] = []
words x = takeWhile (not . isSpace) x : words (dropWhile isSpace (dropWhile (not . isSpace) x))
  where isSpace c = c == ' '

unlines :: [String] -> String -- Transforma lista de quebras em string de linhas (contrario do lines)
unlines [] = ""
unlines (x:xs) = x ++ "\n" ++ unlines xs

unwords :: [String] -> String -- Junta palavras com espaços
unwords [] = ""
unwords [x] = x
unwords (x:xs) = x ++ " " ++ unwords xs 

transpose :: [[a]] -> [[a]] -- pega cada elemento e transforma em uma lista
transpose [] = []
transpose xs
    | all null xs = []
    | otherwise = map head xs : transpose (map tail xs) -- pega a "cabeça" de cada lista, com a nova lista, aplica o processo de forma recursiva até o caso base

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome x = x == reverse x -- fará uma comparação entre x e o x aplicado a função reverse

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

