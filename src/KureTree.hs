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

import Data.Time.Calendar               -- package: time

import qualified Data.Map as Map        -- package: containers
-- import qualified Data.Tree as Tree


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

type Name = String


newtype Sites = Sites { getSites :: [Site] }
  deriving (Eq,Ord,Show)

data Site = Site
    { site_name         :: Name
    , site_children     :: [Tree]
    }
  deriving (Eq,Ord,Show)



data Tree = Element Name Attrs
          | Loc    Name [Tree]
  deriving (Eq,Ord,Show)

newtype Attrs = Attrs { getAttrs :: Map.Map String Value }
  deriving (Eq,Ord,Show)

data Value = String     String
           | Int        Int
           | Float      Double
           | Date       Day
           | BadValue   String
  deriving (Eq,Ord,Show)


attrs :: [(String,Value)] -> Attrs
attrs = Attrs . Map.fromList

treeLabel :: Tree -> Name
treeLabel (Element lbl _)       = lbl
treeLabel (Loc lbl _)          = lbl


-- NOTE - there isn't any recursion except in tree, 
-- perhaps this is overload...
data U = USS Sites | US Site | UT Tree

instance Injection Sites U where
  inject = USS
  project u = do { USS t <- return u; return t }

instance Injection Site U where
  inject = US
  project u = do { US t <- return u; return t }

instance Injection Tree U where
  inject = UT
  project u = do { UT t <- return u; return t }



-- congruence combinator
sitesT :: (Monad m) 
        => Transform c m Site a -> ([a] -> b) -> Transform c m Sites b
sitesT t f = transform $ \c -> \case
    Sites xs -> f <$> mapM (\fo -> applyT t c fo) xs


sitesAllR :: (Monad m) => Rewrite c m Site -> Rewrite c m Sites
sitesAllR r = sitesT r Sites



-- congruence combinator
siteT :: (ExtendPath c Name, Monad m) 
      => Transform c m Tree a -> (Name -> [a] -> b) -> Transform c m Site b
siteT t f = transform $ \c -> \case
    Site lbl ks -> let c1 = c @@ lbl in f lbl <$> mapM (\fo -> applyT t c1 fo) ks


siteAllR :: (ExtendPath c Name, Monad m) => Rewrite c m Tree -> Rewrite c m Site
siteAllR r = siteT r Site


-- Congruence combinator                     
elementT :: Monad m => (Name -> Attrs -> b) -> Transform c m Tree b
elementT f = contextfreeT $ \case
    Element lbl vals -> return (f lbl vals)
    _ -> fail "not an Element"


-- congruence combinator
-- Note - context not proper handled yet!
locT :: (ExtendPath c Name, Monad m) 
      => Transform c m Tree a -> (Name -> [a] -> b) -> Transform c m Tree b
locT t f = transform $ \c -> \case
    Loc lbl ks -> let c1 = c @@ lbl in f lbl <$> mapM (\fo -> applyT t c1 fo) ks
    _ -> fail "not a Loc"
                             
locAllR :: (ExtendPath c Name, Monad m) => Rewrite c m Tree -> Rewrite c m Tree
locAllR r = locT r Loc



instance (ExtendPath c Name) => Walker c U where
  allR :: MonadCatch m => Rewrite c m U -> Rewrite c m U
  allR r = modExc (stackStrategyFailure "allR") $
           rewrite $ \c -> \case
             USS o -> USS <$> applyR allSites c o
             US o  -> US  <$> applyR allSite c o
             UT o  -> UT  <$> applyR allTree c o
    where
      allSites  = readerT $ \_ -> sitesAllR (extractR r)
      allSite   = readerT $ \_ -> siteAllR (extractR r)
      allTree   = readerT $ \case 
                      Element {} -> idR
                      Loc {} -> locAllR (extractR r)




instance (ExtendPath c Name) => Walker c Tree where
  allR :: MonadCatch m => Rewrite c m Tree -> Rewrite c m Tree
  allR r = modExc (stackStrategyFailure "allR") $
           rewrite $ \cx fo -> inject <$> applyR allRtree cx fo
    where
      allRtree = readerT $ \case 
                      Element {} -> idR
                      Loc {} -> locAllR (extractR r)



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
buildTree1 :: [String] -> Attrs -> Tree
buildTree1 keys obj = descend keys 
  where
    descend []      = Element "" obj       
    descend [x]     = Element x obj
    descend (x:xs)  = Loc x [descend xs]


addAtLevel :: String -> Attrs -> [Tree] -> [Tree]
addAtLevel k v kids = step kids
  where 
   step (n:ns) 
        | k < treeLabel n       = Element k v : n : ns
        | otherwise             = n : step ns
   step []                      = [Element k v]


addLeaf :: [String] -> Attrs -> Tree -> Tree
addLeaf []     _   tree                   = tree
addLeaf _      _   leaf@(Element {})      = leaf
addLeaf (p:ps) obj tree@(Loc lbl childs)  = 
    if p == lbl then Loc lbl (step1 ps childs) else tree
  where
    step1 []        kids   = kids
    step1 [k1]      kids   = addAtLevel k1 obj kids
    step1 keys      []     = descend keys
    step1 (k1:keys) kids@(Loc x kids1 : xs) 
       | k1 < x           = let branch1 = descend keys in (Loc k1 branch1 : kids)
       | k1 == x          = let branch1 = step1 keys kids1 in (Loc x branch1 : xs)
       | otherwise        = Loc x kids1 : step1 (k1:keys) xs

    step1 (k1:keys) kids@(Element x vals : xs) 
       | k1 < x           = let branch1 = descend keys in (Loc k1 branch1 : kids)
       | otherwise        = Element x vals : step1 (k1:keys) xs
                          

    descend []           = [] -- unreachable (??)
    descend [k]          = [Element k obj]
    descend (k:ks)       = [Loc k (descend ks)]
    

treeFromLeafList :: [(String,Attrs)] -> Maybe Tree
treeFromLeafList []                     = Nothing
treeFromLeafList ((key1,val1):leaves)   = 
    Just $ foldr fn tree1 leaves
  where
    tree1 = let (root,keys) = keyListProp key1 in buildTree1 (root:keys) val1
    fn (path,obj) ac = 
        let (root,keys) = keyListProp path in addLeaf (root:keys) obj ac 