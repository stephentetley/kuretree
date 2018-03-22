{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  KureTree
-- Copyright   :  (c) Stephen Tetley 2018
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Placeholder...
--
--------------------------------------------------------------------------------


module KureTree where

import Language.KURE                    -- package: kure

import Text.JSON                        -- package: json
import Text.JSON.String

type Row = [(String,String)]



readJsonFlat :: GetJSON [Row]
readJsonFlat = readJSValue >>= descend
  where
    extrResult (Ok a)     = return a
    extrResult (Error s)  = fail s

    descend :: JSValue -> GetJSON [Row]
    descend (JSArray xs)  = extrResult $ jsonRows xs
    descend _             = fail $ "Unable to parse json array"

jsonRows :: [JSValue] -> Result [Row]
jsonRows = post [] . map jsonRow 
   where
     post ac []                 = Ok (reverse ac)
     post ac (Ok r1 : xs)       = post (r1:ac) xs
     post _  (Error s:_)        = Error s


jsonRow :: JSValue -> Result Row
jsonRow obj@(JSObject {})  = decJSDict "jsonRow" obj
jsonRow _                  = Error "jsonRow"