{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  RoseTree
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

module RoseTree where

import Language.KURE                    -- package: kure

data RoseTree a = Node a [RoseTree a]
  deriving (Show)



instance Walker c (RoseTree a) where
   allR :: MonadCatch m => Rewrite c m (RoseTree a) -> Rewrite c m (RoseTree a)
   allR r = modExc (stackStrategyFailure "allR") $
            nodeAllR r <+> idR
            

---------------------------------------------------------------------------

nodeT :: (MonadThrow m) 
     => Transform c m (RoseTree a) a1 -> (a -> [a1] -> b) -> Transform c m (RoseTree a) b
nodeT t f = transform $ \c -> \case
    Node a kids -> f a <$> mapM (\x -> applyT t c x) kids


nodeAllR :: (MonadThrow m) => Rewrite c m (RoseTree a) -> Rewrite c m (RoseTree a)
nodeAllR r1 = nodeT r1 Node

nodeAnyR :: (MonadCatch m) => Rewrite c m (RoseTree a) -> Rewrite c m (RoseTree a)
nodeAnyR r1 = unwrapAnyR $ nodeAllR (wrapAnyR r1) 

nodeOneR :: (MonadCatch m) => Rewrite c m (RoseTree a) -> Rewrite c m (RoseTree a)
nodeOneR r1 = unwrapOneR $ nodeAllR (wrapOneR r1) 

---------------------------------------------------------------------------   