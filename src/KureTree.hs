{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
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

import Data.List.Split                  -- package: split





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

--------------------------------------------------------------------------------
-- 



data Tree lbl o = Leaf lbl o 
                | Node lbl [Tree lbl o]
            deriving (Eq,Ord,Show)

getLabel :: Tree lbl o -> lbl
getLabel (Leaf lbl _)   = lbl
getLabel (Node lbl _)   = lbl


-- Congruence combinator                     
leafT :: Monad m => (lbl -> o -> b) -> Transform c m (Tree lbl o) b
leafT f = contextfreeT $ \case
    Leaf lbl o -> return (f lbl o)
    _         -> fail "not a Leaf"


-- congruence combinator
-- Note - context not proper handled yet!
nodeT :: (ExtendPath c lbl, Monad m) 
        => Transform c m (Tree lbl o) a -> (lbl -> [a] -> b) -> Transform c m (Tree lbl o) b
nodeT t f = transform $ \c -> \case
    Node lbl ks -> let c1 = c @@ lbl in f lbl <$> mapM (\fo -> applyT t c1 fo) ks
    _ -> fail "not a Node"
                             

nodeAllR :: (ExtendPath c lbl, Monad m) => Rewrite c m (Tree lbl o) -> Rewrite c m (Tree lbl o)
nodeAllR r = nodeT r Node


instance (ExtendPath c lbl) => Walker c (Tree lbl o) where
  allR :: MonadCatch m => Rewrite c m (Tree lbl o) -> Rewrite c m (Tree lbl o)
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \cx fo -> inject <$> applyR allRtree cx fo
    where
      allRtree = readerT $ \case 
                      Leaf {} -> idR
                      Node {} -> nodeAllR (extractR r)



--------------------------------------------------------------------------------
-- 


rootName :: String -> String
rootName = build1 . splitOn "/"
  where
    build1 (a:b:_)  = a ++ "/" ++ b 
    build1  _       = ""

childNodeNames :: String -> [String]
childNodeNames = drop 2 . splitOn "/"

-- | Note - the input data does not serialize the nodes in the tree
-- just the end leaves.
buildTree1 :: String -> Tree String () 
buildTree1 path = Node (rootName path) (getKids $ childNodeNames path)
  where
    getKids []      = []    
    getKids [x]     = [Leaf x ()]
    getKids (x:xs)  = [Node x (getKids xs)]


addAtLevel :: String -> obj -> [Tree String obj] -> [Tree String obj]
addAtLevel k v kids = step kids
  where 
   step (n:ns) | k < getLabel n = Leaf k v : n : ns
               | otherwise      = n : step ns
   step []                      = [Leaf k v]


addLeaf :: [String] -> obj -> Tree String obj -> Tree String obj
addLeaf (p:ps) obj tree@(Node lbl kids) = 
    if p == lbl then Node lbl (step1 ps kids) else tree
  where
    step1 []     ks = ks
    step1 [x]    ks = addAtLevel x obj ks
    step1 (x:xs) [] = undefined
    