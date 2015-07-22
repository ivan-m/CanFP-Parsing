---
title: 'l7:Parsing7:Bencodee'
author: Ivan Lazar Miljenovic
date: 22 July, 2015
...


About this talk
===============

## Feel free to yell out! {data-background="images/yell.jpg" data-background-color="white"}

###### [Picture](https://www.flickr.com/photos/otacke/12635014673) by Oliver Tacke / [CC BY]

## Who is this guy? {data-background="images/huh.jpg" data-background-color="lightblue"}

###### [Picture](https://www.flickr.com/photos/jumpn_around/2239989214/) by elio m. / [CC BY-NC]

Notes
:   * Using Haskell since late 2007
    * Maintainer of various libraries, primarily graph related
    * Writing my own parser
    * Name very Google-able

## What's going to be covered?

> * Write a simple parser
> * Define data types
> * Encounter the dreaded "M"-word
> * Survive doing so

## Want to skip ahead?

Slides
:   <http://ivan-m.github.io/CanFP-Parsing/>
:   (Hint: press `s` to see my secret notes!)

Source Code
:   <https://github.com/ivan-m/CanFP-Parsing/tree/master/src>

Why Parser Combinators?
=======================

## Consider a common alternative...

Notes
:   * LCA 2013, Programming Miniconf
    * "Solving Interesting Problems by Writing Parsers" by Jacinta
      Richardson
    * Videos available
    * Perl-focussed
    * So it should be no surprise when she decided to use...

## {#regex-horror data-background="images/horror.jpg" data-background-color="lightblue"}

> **Every time you mention regular expressions, someone brings up
> Jamie Zawinski.**

. . .

> **Now you have two problems.**

###### [Picture](https://www.flickr.com/photos/jeremybrooks/2199355153/) by Jeremy Brooks / [CC BY-NC]

Notes
:   * She even admits regexes are often the wrong solution.
    * Haskellers like Erik de Castro Lopo were watching

## Why not regexes?

> * Stringly-typed
> * Not re-usable
> * Difficult to read
> * Re-interpreted by compiler

Notes
:   * Embeddable regexes, quasiquotes, etc.

## Compare the pair

Regular Expressions
:   ```haskell
    "\.\([[:alpha:]][[:alnum:]-_:\.]*\)"
    ```

Parser Combinators
:   ```haskell
    identifierAttr = do
      char '.'
      first <- letter
      rest <- many $ alphaNum
                     <|> oneOf "-_:."
      return (first:rest)
    ```

Notes
:   * Sample taken from Pandoc, identifier in attribute
    * regex shorter
    * Have I matched all the parens properly?
    * combinator version could be shorter
    * Which is more readable? combinable?
    * regexes more convenient for custom munging
    * I forgot the `char '.'`; which is it easier to spot in?

## What about Parser Generators?

Notes
:    * *Requires* a grammar
     * Combinators easier to extend
     * Probably faster
     * Can emulate in a parser combinator
     * Not embeddable in code
     * Usually needs an external tool

## Availability of parser combinators

* Most (all?) FP languages
* Javascript
* R
* Java
* C#
* etc.

. . .

> **Not just for data!**

Notes
:   * Multiple implementations (parsec, attoparsec, polyparse,
      trifecta, etc.)
    * Rite of passage!
    * If Java has it, *of course* C# has to have it to prove they're
      better...

Bencode
=======

## What is Bencode?

> * Encoding used by BitTorrent for storing/transmitting data
> * Comprised of four different types of values:

. . .

Type                Example
------------  --------------------------
Integers              `i42e`
Strings              `4:spam`
Lists             `l4:spami42ee`
Dictionaries   `d3:bar4:spam3:fooi42ee`


Notes
:   * Suggested by Mats H
    * Dictionaries require keys to be strings

## Simplifications

* Use normal strings, not byte strings
* No validation checks (e.g. allow `-0`)
* Ignore dictionary ordering

Notes
:   * Strings should be a sequence of bytes
    * Dictionaries should be lexicographically ordered by keys

## Datatypes are Awesome! {data-background="images/awesome.jpg" data-background-color="lightblue"}

. . .

~~~haskell
data Bencode = BInt Int
             | BString String
             | BList [Bencode]
             | BDict [(String, Bencode)]
             deriving (Show)
~~~

###### [Picture](https://www.flickr.com/photos/33774513@N08/3153440876/) by crises_crs / [CC BY-NC]

Defining the Parser
===================

## What is a parser?


~~~haskell
type Parser a = String -> a
~~~

Notes
:   * Most basic definition of what a parser is
    * Is that it?

## Consider parsing an integer

> * Let's parse `i42e`
> * Imagine a function `next :: Parser Char`

Notes
:   * I first need to get the first char to get `i`
    * Now what?
    * The input is all used up!

## Attempt 2

~~~haskell
type Parser a = String -> (a, String)
~~~

Notes
:   * Returns un-consumed input
    * What happens if it _isn't_ an integer?
    * This says it always returns a value!

## Attempt 3

~~~haskell
-- The result of a parser: either an error or the expected result.
data Result a = Err String
              | OK a
              deriving (Show)

type Parser a = String -> (Result a, String)
~~~

Notes
:   * Up to isomorphism of how result is returned
    * Complete in terms of being able to work
    * Can be bypassed

## Final definition


~~~haskell
newtype Parser a = P { runP :: String -> (Result a, String) }
~~~

. . .


~~~haskell
-- Shhhhhh!!!!!
newtype State s a = St { runState :: s -> (a, s) }
~~~

Notes
:   * Don't export constructor
    * `runP` runs the parser
    * `newtype` is run-time isomorphic to original
    * Specialised version of State Monad

Let's start parsing!
====================

## Create basic parsers

~~~haskell
-- Lift a value into a parser.
toParser :: a -> Parser a
toParser a = P $ \str -> (OK a, str)

-- Throw a parser error.
failParser :: String -> Parser a
failParser err = P $ \str -> (Err err, str)
~~~

## Get some data

~~~haskell
-- Obtain the next character in the input string.
next :: Parser Char
next = P $ \str -> case str of
                     c:str' -> (OK c, str')
                     _      -> (Err "empty", str)
~~~

## Matching a predicate

~~~haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \str ->
  case str of
    c:str' | p c       -> (OK c, str')
           | otherwise -> (Err "not satisfied", str)
    _                  -> (Err "empty", str)

-- For example: parse the specified character.
char :: Char -> Parser Char
char c = satisfy (c==)
~~~

Notes
:   * When parsing an integer, we want `isDigit` (see later)
    * This looks an awful lot like `next`
    * Idea: call next, get the result, then act on it.

## Matching again

~~~haskell
-- Take the result from one parser, and pass it as a parameter to a
-- function that returns a parser.
inject :: Parser a -> (a -> Parser b) -> Parser b
inject pa fpb = P $ \str -> case runP pa str of
                              (OK a,  str') -> runP (fpb a) str'
                              (Err e, str') -> (Err e, str')

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = next `inject` checkNext
  where
    checkNext c
      | p c       = toParser c
      | otherwise = failParser "not satisfied"
~~~

Notes
:   * Better!
    * `inject` is a handy concept

## Getting multiple digits

~~~haskell
import Data.Char(isDigit)

atLeastOnce :: Parser a -> [Parser a]
atLeastOnce = -- to be implemented

parseIntDigits :: Parser String
parseIntDigits = atLeastOnce (satisfy isDigit)
~~~

Notes
:   * Ignoring negative numbers for now
    * Sample code has it implemented though

## But I want a number!

Need to convert a `String` to an `Int`...

. . .

~~~haskell
-- Apply a function on the result of a parser.
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f pa = P $ \str -> case runP pa str of
                               (OK a,  str') -> (OK (f a), str')
                               (Err e, str') -> (Err e, str')

parseInt :: Parse Int
parseInt = mapParser read parseIntDigits
~~~

Notes
:   * Talk about `read`
    * Real code, might use something better

## That function looks familiar

~~~haskell
mapParser :: (a -> b) -> Parser a  -> Parser b

map       :: (a -> b) ->       [a] ->       [b]

-- Let's generalise

mapF      :: (a -> b) -> f      a  -> f      b
~~~

## That's Func-tastic!

~~~haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Parser where
  fmap = mapParser
~~~

With the handy in-fix alias of `<$>`.

Notes
:   * `<$>` vs `$`
    * Functor laws

## Discarding

Imagine these combinators:

~~~haskell
discLeft :: Parser a -> Parser b -> Parser b

discRight :: Parser a -> Parser b -> Parser a

-- Run a parser on either side of the one specified.
bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket pb pk pa = pb `discLeft` (pa `discRight` pk)

parseBInt :: Parser Bencode
parseBInt = BInt <$> bracket (char 'i') (char 'e') parseInt
~~~

Notes
:   * The discard functions can be obtained by lifting `const` and
      `flip const` up into the `Parser`.

## Applying lifted functions

~~~haskell
applyFunc :: Parser (a -> b) -> Parser a -> Parser b
applyFunc pf pa = pf `inject`
                      (\f -> pa `inject`
                                (\a -> toParser (f a)))

discLeft :: Parser a -> Parser b -> Parser b
discLeft pa pb = (flip const <$> pa) `applyFunc` pb

discRight :: Parser a -> Parser b -> Parser a
discRight pa pb = (const <$> pa) `applyFunc` pb
~~~

Notes
:   * This would look neater if I set up the proper fixities (less
      parens)
    * But this way it's more obvious

## Introducing Applicative!

~~~haskell
class Functor f => Applicative f where
    -- Lift a value.
    pure :: a -> f a

    -- Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Parser where
  pure = toParser
  (<*>) = applyFunc

bracket :: Parser bra -> Parser ket -> Parser a -> Parser a
bracket pb pk pa = pb *> pa <* pk
~~~

Notes
:   * `*>` and `<*` pre-defined, along with other combinators
    * `parseBInt` doesn't change

## We still need to get the digits

> 1. Try our parser `p`.
> 2. If it fails, no values, so return an empty list `[]`.
> 3. Otherwise, recurse and put this value on the front of the list.

Notes
:   * We want to get a list of values
    * We can handle putting the value on front with Applicative

## Handling backtracking

> 1. `try`-based semantics
> 2. `commit`-based semantics
> 3. "Just do it already"-based semantics

Notes
:   * Three differeint ways of handling backtracking
    * Forget the `try`? No backtracking
    * Forget the `commit`? Still works, just less performance and
      worse errors (*"Partial Parsing: combining choice with
      commitment"* by Malcolm Wallace)
    * Doing it, as epitomised by `attoparsec`.

## Failing gracefully

~~~haskell
-- If the first parser fails, try the second.
onFail :: Parser a -> Parser a -> Parser a
onFail p1 p2 = P $ \str -> case runP p1 str of
                             (Err _, _) -> runP p2 str
                             ok         -> ok
~~~

## Trust me, there's a class for it

~~~haskell
class Applicative f => Alternative f where
    -- The identity of '<|>'
    empty :: f a
    -- An associative binary operation
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = failParser "empty value"
  (<|>) = onFail

-- Try to parse one of the provided parsers in sequence.
oneOf :: [Parser a] -> Parser a
oneOf []     = fail "No parsers remaining"
oneOf (p:ps) = p <|> oneOf ps
~~~

Notes
:   * This class comes from parser libraries
    * Parsec still (?) uses it's own `<|>`.
    * Comes with `many` and `some` (which I called `atLeastOnce`).

## Finally, we have Integers!

~~~haskell
parseInt :: Parse Int
parseInt = read <$> some (satisfy isDigit)

parseBInt :: Parser Bencode
parseBInt = BInt <$> bracket (char 'i') (char 'e') parseInt
~~~

## Time for Strings

> 1. Parse a (non-negative) integer `n`
> 2. Parse a `:`
> 3. Return the next `n` characters

. . .


~~~haskell
parseString :: Parser String
parseString = parseInt
              `inject`
              (\n -> char ':' *> exactly n next)

-- Run the parser the specified number of times.
exactly :: Int -> Parser a -> Parser [a]
exactly n p
  | n <= 0    = pure []
  | otherwise = liftA2 (:) p (exactly (n-1) p)
~~~

## I wish there was a nicer syntax for inject...

. . .

> But there is!

## Warm Fuzzy Things

> * aka _Workflows_
> * ~~aka _Burritos_~~
> * aka "a monoid in the category of endofunctors"
> * aka <span style="font-variant:small-caps;">_Monads_</span>

Notes
:   * Workflows are from F#
    * SPJ: Our biggest mistake: Using the scary term "monad" rather than
      "warm fuzzy thing" (Wearing the hair shirt: a retrospective on
      Haskell (2003))
    * "monoid" from "A Brief, Incomplete, and Mostly Wrong History of
      Programming Languages" by James Iry (supposedly Philip Wadler)
    * Burritos from: Abstraction, intuition, and the “monad tutorial
      fallacy” by Brent Yorgey

## The "M" word

~~~haskell
class Applicative m => Monad m where
    -- Usually called "bind"
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a

instance Monad Parser where
  (>>=) = inject
  (>>) = (*>)
  return = pure
  fail = failParser
~~~

Notes
:   * `Applicative` constraint as of GHC 7.10 (AMP)
    * `return` and `(*>)` have these by default.
    * `fail` is considered a wart; used for pattern matching failures
    * Replace `fail` with a new class (e.g. `MonadPlus`)?
    * Parsers possibly only semi-valid use of `fail`.

## Strings again

~~~haskell
parseString :: Parser String
parseString = do n <- parseInt
                 char ':'
                 exactly n next

parseBString :: Parser Bencode
parseBString = BString <$> parseString
~~~

Notes
:   * Sequential-style operations!
    * "In short, Haskell is the world’s finest imperative programming
      language." (SPJ, "Tackling the Awkward Squad")
    * `a <- m; ...` equivalent to `m >>= (\a -> ...)`
    * `m1; m2` equivalent to `m1 >> m2`

## Finishing it off

~~~haskell
parseBList :: Parser Bencode
parseBList = BList <$> bracket (char 'l') (char 'e')
                               (many parseBencode)

parseBDict :: Parser Bencode
parseBDict = BDict <$> bracket (char 'd') (char 'e') (many parseKV)
  where
    parseKV = liftA2 (,) parseString parseBencode

parseBencode :: Parser Bencode
parseBencode = oneOf [ parseBInt, parseBString
                     , parseBList, parseBDict
                     ]
~~~

Notes
:   * Mutual recursion is fun!

## Trying it out

~~~haskell
λ> runP parseBencode "l7:Parsing7:Bencodee"
(OK (BList [BString "Parsing",BString "Bencode"]),"")
~~~

Where to from here?
===================

## Improve current parser

* Negative numbers
* Ensure `-0` is illegal
* Strings can't have negative length
* Parse bytes, not characters
* Improve error messages

Notes
:   * Source already supports -ve numbers

## Write a better Parser?

> **Datatypes are awesome...**

. . .

> **... but functions are even *more* awesome!**

Notes
:   * Avoid context-switching on the `Result` type.

## Continuation Parsing Style

Some helper definitions first:

~~~haskell
type Failure   r = String -> ErrMsg -> Result r
type Success a r = String -> a      -> Result r

data Result a = Err String ErrMsg
              | OK String a
              deriving (Eq, Ord, Show, Read)

-- To avoid mixing our types up.
type ErrMsg = String
~~~

Notes
:   * Note different definition of `Result`.
    * Based loosely upon `attoparsec`

## CPS Parser

~~~haskell
{-# LANGUAGE RankNTypes #-}

newtype Parser a = P {
      runP :: forall r. String
           -> Failure   r
           -> Success a r
           -> Result r
    }
~~~

## Running a CPS Parser

~~~haskell
failK :: Failure a
failK str msg = Err str msg

successK :: Success a a
successK str a = OK str a

runParser :: Parser a -> String -> Result a
runParser p str = runP p str failK successK
~~~

## Exercise

> **Complete the CPS parser definition!**

. . .

> **Once you have the instances and `next` defined, the rest should be
> (almost) identical.**

Notes
:   Source definition not yet complete, will be Real Soon Now (TM).

## So long and thanks for all the fish! {data-background="images/fish.jpg" data-background-color="lightblue"}

###### [Picture](https://www.flickr.com/photos/zest-pk/923931403) by Zest-pk / [CC BY]

---
# License links
...

[CC BY]: https://creativecommons.org/licenses/by/2.0/
[CC BY-NC]: https://creativecommons.org/licenses/by-nc/2.0/

---
# reveal.js settings
theme: night
transition: concave
backgroundTransition: zoom
center: true
history: true
css: custom.css
...
