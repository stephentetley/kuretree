{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Demo02 where

import Language.KURE

import BinaryTree

tree1 :: BinaryTree Int
tree1 = Bin 1 (Bin 2 Empty Empty) Empty

-- | For this simple example, the context is just an 'AbsolutePath', and transformations always operates on 'Arith'.
type TransformA a b = Transform () KureM (BinaryTree a) b
type RewriteA a = TransformA a (BinaryTree a)


applyTransform :: TransformA a b -> (BinaryTree a) -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty

demo01 :: Either String (BinaryTree Int)
demo01 = applyTransform idR tree1

add1 :: RewriteA Int
add1 = withPatFailExc (nodeMismatch "addLitR") $
          do Bin a x y <- idR
             return (Bin (a + 1) x y)
             
demo02 :: Either String (BinaryTree Int)
demo02 = applyTransform (alltdR (add1 <+> idR)) tree1             

demo02a :: Either String (BinaryTree Int)
demo02a = applyTransform add1 tree1  
