{- |
   Module      : SimpleParser
   Description : Simple parser definition
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is an extremely simple definition of a parser.

 -}
module SimpleParser
  ( Parser
  , runP
  , parseBencode
  , Bencode (..)
  ) where

import Bencode

import Control.Applicative
import Data.Char

-- -----------------------------------------------------------------------------

-- | Our parser representation.  We can run it with the 'runP' field
-- function.
newtype Parser a = P { runP :: String -> (Result a, String) }

-- | The result of a parser: either an error or the expected result.
data Result a = Err String
              | OK a
              deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------
-- Instances

instance Functor Parser where
  fmap = mapParser

instance Applicative Parser where
  pure = toParser
  (<*>) = applyFunction

instance Monad Parser where
  return = toParser
  (>>=) = inject
  fail = failParser

instance Alternative Parser where
  empty = failParser "empty value"
  (<|>) = onFail

-- -----------------------------------------------------------------------------
-- Functions for defining instances.

-- | Lift a value into a parser.
toParser :: a -> Parser a
toParser a = P $ \str -> (OK a, str)

-- | Throw a parser error.
failParser :: String -> Parser a
failParser err = P $ \str -> (Err err, str)

-- | Apply a function on the result of a parser.
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f pa = P $ \str -> case runP pa str of
                               (OK a,  str') -> (OK (f a), str')
                               (Err e, str') -> (Err e, str')

-- | Apply a function obtained from a parser to a value from another
-- parser.
applyFunction :: Parser (a -> b) -> Parser a -> Parser b
applyFunction pf pa = do f <- pf
                         a <- pa
                         return $ f a

-- | Take the result from one parser, and pass it as a parameter to a
-- function that returns a parser.
inject :: Parser a -> (a -> Parser b) -> Parser b
inject pa fpb = P $ \str -> case runP pa str of
                              (OK a,  str') -> runP (fpb a) str'
                              (Err e, str') -> (Err e, str')

-- | If the first parser fails, try the second.
onFail :: Parser a -> Parser a -> Parser a
onFail p1 p2 = P $ \str -> case runP p1 str of
                             (Err _, _) -> runP p2 str
                             ok         -> ok

-- -----------------------------------------------------------------------------
-- Higher-order combinators

-- | Run a parser on either side of the one specified.
bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket pb pk pa = pb *> pa <* pk

-- | Run the parser the specified number of times.
exactly :: Int -> Parser a -> Parser [a]
exactly n p
  | n <= 0    = pure []
  | otherwise = liftA2 (:) p (exactly (n-1) p)

-- | Try to parse one of the provided parsers in sequence.
oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) (fail "No parsers remaining")

-- -----------------------------------------------------------------------------
-- Defining low-level helper functions.

-- | Obtain the next character in the input string.
next :: Parser Char
next = P $ \str -> case str of
                     c:str' -> (OK c, str')
                     _      -> (Err "empty", str)

-- | Only return the next character if it satisfies the provided
-- predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- next
               if p c
                  then toParser c
                  else failParser "not satisfied"

{-

Alternate definitions of satisfy:

-- | Define 'satisfy' using higher-order functions.
satisfyHigher :: (Char -> Bool) -> Parser Char
satisfyHigher p = next `inject` checkNext
  where
    checkNext c
      | p c       = toParser c
      | otherwise = failParser "not satisfied"

-- | Explicitly define 'satisfy'.
satisfyExplicit :: (Char -> Bool) -> Parser Char
satisfyExplicit p = P $ \str ->
  case str of
    c:str' | p c       -> (OK c, str')
           | otherwise -> (Err "not satisfied", str)
    _                  -> (Err "empty", str)

-}

-- | Parse the specified character.
char :: Char -> Parser Char
char c = satisfy (c==)

-- -----------------------------------------------------------------------------
-- Parsing integers

parseInt :: Parser Int
parseInt = do mn <- optional (char '-')
              ds <- some (satisfy isDigit)
              return $ read (maybe id (:) mn ds)

parseBInt :: Parser Bencode
parseBInt = BInt <$> bracket (char 'i') (char 'e') parseInt

-- -----------------------------------------------------------------------------
-- Parsing strings

parseString :: Parser String
parseString = do n <- parseInt
                 if n >= 0
                    then char ':' *> exactly n next
                    else fail "string must have non-negative length"

parseBString :: Parser Bencode
parseBString = BString <$> parseString

-- -----------------------------------------------------------------------------
-- Parsing lists

parseBList :: Parser Bencode
parseBList = BList <$> bracket (char 'l') (char 'e') (many parseBencode)

-- -----------------------------------------------------------------------------
-- Parsing dictionaries

parseBDict :: Parser Bencode
parseBDict = BDict <$> bracket (char 'd') (char 'e') (many parseKV)
  where
    parseKV = liftA2 (,) parseString parseBencode

-- -----------------------------------------------------------------------------
-- Putting it all together

parseBencode :: Parser Bencode
parseBencode = oneOf [ parseBInt
                     , parseBString
                     , parseBList
                     , parseBDict
                     ]
