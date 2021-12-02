-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
--import qualified Data.DList as D
-- Basic definitions

newtype Parser a = P (String -> MyList (a,String))

{-============================================================================-}
{-==== this part helps try out different data structures. Can be ignored =====-}
{-============================================================================-}
type MyList a = [a]--D.DList a

myToList :: MyList a -> [a]
myToList = id--D.toList

mySingleton :: a -> MyList a
mySingleton = (\x -> [x]) -- D.singleton

myEmpty :: MyList a
myEmpty = []--D.empty

myFold :: (a -> b -> b) -> b -> MyList a -> b
myFold = foldr --D.foldr

myConcat :: [MyList a] -> MyList a
myConcat = concat--D.concat

myAppend :: MyList a -> MyList a -> MyList a
myAppend = (++) --D.append
{-============================================================================-}
{-============================================================================-}
match :: Parser a -> String -> MyList (a,String)
match (P p) inp = p inp

parse :: Parser a -> String -> [(a,String)]
parse p inp = myToList $ match p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> myEmpty
                     (x:xs) -> mySingleton (x,xs))
{-============================================================================-}
tupleL :: (a -> b) -> (a,c) -> (b,c)
tupleL f (x,y) = (f x, y)

endOfStr :: Parser ()
endOfStr = P(\inp -> if null inp then mySingleton ((),inp) else myEmpty )
{-============================================================================-}
-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> (tupleL g)<$>(match p inp))
    {-case parse p inp of
                            []  -> []
                            [(v,out)] -> [(g v, out)])-}

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> mySingleton (v,inp) )

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp ->  (funcs inp) <*> (outputs inp) )
     where  results x = match pg x -- [((a -> b), string)]
            funcs   x = (tupleL.fst) <$> results x -- [((a,c) -> (b,c))]
            outputs x = myConcat $(match px.snd)<$>results x --[(a,string)]

                             {-case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)-}

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp ->   myConcat $ zipWith ($) (myToList$(match.f)<$>values inp) (myToList$outputs inp) )
      where  results x = match p x -- [(a,String)]
             values  x = fst<$>results x -- [a]
             outputs x = snd<$>results x -- [String]
             {-case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out) -}

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> myEmpty)

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> let x = match p inp in if null (myToList x) then match q inp else x)

                          {-case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)]) -}
instance Semigroup (Parser a) where
  --(<>) :: Parser a -> Parser a ->  Parser a
  p <> q = P (\inp -> match p inp <> match q inp)




-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
