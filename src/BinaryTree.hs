{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BinaryTree
-- Copyright   :  (c) Stephen Tetley 2019
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Placeholder...
--
--------------------------------------------------------------------------------

module BinaryTree where

import Language.KURE                    -- package: kure
import Text.JSON                              -- pacake: json

import Control.Monad

data BinaryTree a = Empty | Bin a (BinaryTree a) (BinaryTree a)
  deriving (Show)



instance Walker c (BinaryTree a) where
   allR :: MonadCatch m => Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a)
   allR r = modExc (stackStrategyFailure "allR") $
            binAllR r r <+> idR  

---------------------------------------------------------------------------

-- congruence combinator

emptyT :: MonadThrow m => b -> Transform c m (BinaryTree a) b
emptyT v = contextfreeT $ \case
                           Empty -> pure v
                           _     -> throwM (nodeMismatch "Empty")
                           
---------------------------------------------------------------------------

binT :: (MonadThrow m) 
     => Transform c m (BinaryTree a) a1 -> Transform c m (BinaryTree a) a2 -> (a -> a1 -> a2 -> b) -> Transform c m (BinaryTree a) b
binT t1 t2 f = transform $ \c -> \case
    Bin a x y -> f a <$> applyT t1 c x <*> applyT t2 c y
    _ -> throwM (nodeMismatch "Bin")
    
binAllR :: (MonadThrow m) => Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a)
binAllR r1 r2 = binT r1 r2 Bin

binAnyR :: (MonadCatch m) => Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a)
binAnyR r1 r2 = unwrapAnyR $ binAllR (wrapAnyR r1) (wrapAnyR r2)

binOneR :: (MonadCatch m) => Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a)
binOneR r1 r2 = unwrapOneR $ binAllR (wrapOneR r1) (wrapOneR r2)

---------------------------------------------------------------------------    
instance JSON a => JSON (BinaryTree a) where
  showJSON = toJSValue showJSON
  readJSON = parseValue readJSON
    
  
toJSValue :: (a -> JSValue) -> BinaryTree a -> JSValue
toJSValue _ Empty = JSNull
toJSValue f (Bin a t1 t2) =
    JSObject $ toJSObject [("label", f a), ("left", toJSValue f t1), ("right", toJSValue f t2)]
    
parseValue :: (JSValue -> Result a) -> JSValue -> Result (BinaryTree a)
parseValue _ JSNull = return Empty
parseValue mf (JSObject o) = decompose o
  where
    decompose obj = do 
      label <- valFromObj "label" obj >>= mf
      left  <- valFromObj "left"  obj >>= parseValue mf
      right <- valFromObj "right" obj >>= parseValue mf
      return (Bin label left right)

parseValue _ _ = mzero
    

