#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Main
   Description : Easily write speakers notes for pandoc + reveal.js
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD-style (see the file LICENSE)
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import Text.Pandoc.JSON

-- -----------------------------------------------------------------------------

main :: IO ()
main = toJSONFilter notify

-- -----------------------------------------------------------------------------

notify :: Maybe Format -> Block -> [Block]
notify mfmt (DefinitionList [([Str "Notes"], nts)])
  = case mfmt of
      Just "revealjs" -> RawBlock (Format "html") "<aside class=\"notes\">"
                         : concat nts
                         ++ [RawBlock (Format "html") "</aside>"]
      Just "dzslides" -> [Div ("",[],[("role","note")]) (concat nts)]
      _               -> []
notify _ bl = [bl]
