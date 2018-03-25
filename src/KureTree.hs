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


-- | keyListProp - key list for the propriety data set we are 
-- interested in.
-- The RootName actually is seperated with a forward-slash, 
-- after that forward-slash seperates inividual nodes.
-- 
keyListProp :: String -> (String,[String])
keyListProp = build1 . splitOn "/"
  where
    build1 (a:b:xs)  = (a ++ "/" ++ b, xs)
    build1  xs       = ("",xs)


-- | Note - the input data does not serialize the nodes in the tree
-- just the end leaves.
buildTree1 :: [String] -> obj -> Tree String obj
buildTree1 keys obj = descend keys 
  where
    descend []      = Leaf "" obj       
    descend [x]     = Leaf x obj
    descend (x:xs)  = Node x [descend xs]


addAtLevel :: String -> obj -> [Tree String obj] -> [Tree String obj]
addAtLevel k v kids = step kids
  where 
   step (n:ns) | k < getLabel n = Leaf k v : n : ns
               | otherwise      = n : step ns
   step []                      = [Leaf k v]


addLeaf :: [String] -> obj -> Tree String obj -> Tree String obj
addLeaf []     _   tree                   = tree
addLeaf _      _   leaf@(Leaf {})         = leaf
addLeaf (p:ps) obj tree@(Node lbl childs) = 
    if p == lbl then Node lbl (step1 ps childs) else tree
  where
    step1 []        kids   = kids
    step1 [k1]      kids   = addAtLevel k1 obj kids
    step1 keys      []     = descend keys
    step1 (k1:keys) kids@(Node x kids1 : xs) 
       | k1 < x           = let branch1 = descend keys in (Node k1 branch1 : kids)
       | k1 == x          = let branch1 = step1 keys kids1 in (Node x branch1 : xs)
       | otherwise        = Node x kids1 : step1 (k1:keys) xs

    step1 (k1:keys) kids@(Leaf x val : xs) 
       | k1 < x           = let branch1 = descend keys in (Node k1 branch1 : kids)
       | otherwise        = Leaf x val : step1 (k1:keys) xs
                          

    descend []           = [] -- unreachable (??)
    descend [k]          = [Leaf k obj]
    descend (k:ks)       = [Node k (descend ks)]
    

treeFromLeafList :: [(String,obj)] -> Maybe (Tree String obj)
treeFromLeafList []                     = Nothing
treeFromLeafList ((key1,val1):leaves)   = 
    Just $ foldr fn tree1 leaves
  where
    tree1 = let (root,keys) = keyListProp key1 in buildTree1 (root:keys) val1
    fn (path,obj) ac = 
        let (root,keys) = keyListProp path in addLeaf (root:keys) obj ac 