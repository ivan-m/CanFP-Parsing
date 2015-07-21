---
title: 'l7:Parsing7:Bencodee'
author: Ivan Lazar Miljenovic
date: 22 July, 2015
...


About this talk
===============

## Feel free to yell out! {data-background="images/yell.jpg" data-background-color="white"}

## Who is this guy? {data-background="images/huh.jpg" data-background-color="lightblue"}

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

## Attempt 2

~~~haskell
type Parser a = String -> (a, String)
~~~

Notes
:   * Returns un-consumed input
    * What happens if it _isn't_ an integer?

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
    * Specialised version of State Monad

---
# reveal.js settings
theme: night
transition: concave
backgroundTransition: zoom
center: true
history: true
css: custom.css
...
