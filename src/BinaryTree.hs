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

data BinaryTree a = Empty | Bin a (BinaryTree a) (BinaryTree a)
  deriving (Show)
  
  
-- congruence combinator

emptyT :: MonadThrow m => b -> Transform c m (BinaryTree a) b
emptyT v = contextfreeT $ \case
                           Empty -> pure v
                           _     -> throwM (nodeMismatch "Empty")
                           
                           
binT :: (MonadThrow m) 
     => Transform c m (BinaryTree a) a1 -> Transform c m (BinaryTree a) a2 -> (a -> a1 -> a2 -> b) -> Transform c m (BinaryTree a) b
binT t1 t2 f = transform $ \c -> \case
    Bin a x y -> f a <$> applyT t1 c x <*> applyT t2 c y
    _ -> throwM (nodeMismatch "Bin")
    
binAllR :: (MonadThrow m) => Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a) -> Rewrite c m (BinaryTree a)
binAllR r1 r2 = binT r1 r2 Bin    

    

