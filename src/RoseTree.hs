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
import Text.JSON                        -- package: json

import Control.Monad

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

instance JSON a => JSON (RoseTree a) where
  showJSON = toJSValue showJSON
  readJSON = parseValue readJSON
    
  
toJSValue :: (a -> JSValue) -> RoseTree a -> JSValue
toJSValue f (Node a xs) =
    JSObject $ toJSObject [("node", f a), ("kids", kids)]
  where
    kids = JSArray $ map (toJSValue f) xs

    
parseValue :: (JSValue -> Result a) -> JSValue -> Result (RoseTree a)
parseValue mf (JSObject o) = decompose o
  where
    decompose obj = do 
      label <- valFromObj "node" obj >>= mf
      JSArray xs  <- valFromObj "kids"  obj
      kids <- mapM (parseValue mf) xs
      return (Node label kids)
parseValue _ _ = mzero
