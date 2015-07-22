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
                   runP str fl (\str1 a -> sc str1 (f a))
