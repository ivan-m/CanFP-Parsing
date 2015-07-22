{-# LANGUAGE RankNTypes #-}
{- |
   Module      : CPSParser
   Description : Continuation-based parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   A Continuation-Parsing Style parser, based loosely upon attoparsec.

 -}
module CPSParser
  ( Parser
  , runParser
  , parseBencode
  , Bencode (..)
  ) where

import Bencode

import Control.Applicative
import Data.Char

-- -----------------------------------------------------------------------------

-- | Our parser is more explicitly a function than previously.  Use
-- 'runParser' to run the parser.
newtype Parser a = P {
      runP :: forall r. String
           -> Failure   r
           -> Success a r
           -> Result r
    }

type Failure   r = String -> ErrMsg -> Result r
type Success a r = String -> a      -> Result r

-- | The result of a parser: either an error or the expected result.
-- Unlike the original, this also contains the resulting String.
data Result a = Err String ErrMsg
              | OK String a
              deriving (Eq, Ord, Show, Read)

instance Functor Result where
  fmap _ (Err str e) = Err str e
  fmap f (OK  str a) = OK  str (f a)

-- | To avoid mixing our types up.
type ErrMsg = String

resultToEither :: Result a -> (Either ErrMsg a, String)
resultToEither (Err str e) = (Left e,  str)
resultToEither (OK  str a) = (Right a, str)

-- | Terminal failure continuation.
failK :: Failure a
failK str msg = Err str msg

-- | Terminal success continuation.
successK :: Success a a
successK str a = OK str a

-- | Apply a parser to an input token sequence.
runParser :: Parser a -> String -> (Either ErrMsg a, String)
runParser p str = resultToEither $ runP p str failK successK

-- -----------------------------------------------------------------------------
-- Instances

instance Functor Parser where
  fmap f p = P $ \str fl sc ->
                   runP p str fl (\str1 a -> sc str1 (f a))

instance Applicative Parser where
  pure = returnP

  pf <*> px = do f <- pf
                 x <- px
                 return $ f x

returnP :: a -> Parser a
returnP a = P $ \str _fl sc -> sc str a

instance Alternative Parser where
  empty = fail "no parse"

  p1 <|> p2 = P $ \str fl sc ->
                    let fl' _str' _e = runP p2 str fl sc
                    in runP p1 str fl' sc

instance Monad Parser where
  pa >>= fpb = P $ \str fl sc ->
                     runP pa str fl $ \str1 a ->
                                        runP (fpb a) str1 fl sc

  return = returnP

  fail e = P $ \str fl _sc -> fl str e

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
next = P $ \str fl sc -> case str of
                           c:str' -> sc str' c
                           _      -> fl str "empty"

-- | Only return the next character if it satisfies the provided
-- predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- next
               if p c
                  then return c
                  else fail "not satisfied"

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
