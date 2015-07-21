{- |
   Module      : Bencode
   Description : Bencode definition
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Bencode where

-- -----------------------------------------------------------------------------

-- | Simplified representation of a Bencode value.
data Bencode = BInt Int
             | BString String
             | BList [Bencode]
             | BDict [(String, Bencode)]
             deriving (Eq, Ord, Show, Read)
