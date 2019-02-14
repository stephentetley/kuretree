{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Demo02 where

import Language.KURE

import BinaryTree

tree1 :: BinaryTree Int
tree1 = Bin 1 (Bin 2 (Bin 4 Empty Empty) (Bin 5 Empty Empty)) (Bin 3 Empty Empty)

-- | For this simple example, the context is just an 'AbsolutePath', and transformations always operates on 'Arith'.
type TransformA a b = Transform () KureM (BinaryTree a) b
type RewriteA a = TransformA a (BinaryTree a)


applyTransform :: TransformA a b -> (BinaryTree a) -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty

demo01 :: Either String (BinaryTree Int)
demo01 = applyTransform idR tree1

add1 :: RewriteA Int
add1 = withPatFailExc (nodeMismatch "add1") $
          do Bin a x y <- idR
             return (Bin (a + 1) x y)

demo02 :: Either String (BinaryTree Int)
demo02 = applyTransform add1 tree1

         

demo03 :: Either String (BinaryTree Int)
demo03 = applyTransform (oneR (add1 <+> idR)) tree1             

demo03b :: Either String (BinaryTree Int)
demo03b = applyTransform (oneR add1) tree1

demo04 :: Either String (BinaryTree Int)
demo04 = applyTransform (anyR (add1 <+> idR)) tree1

demo04b :: Either String (BinaryTree Int)
demo04b = applyTransform (anyR add1) tree1

demo05 :: Either String (BinaryTree Int)
demo05 = applyTransform (allR (add1 <+> idR)) tree1

demo05b :: Either String (BinaryTree Int)
demo05b = applyTransform (allR add1) tree1


demo06 :: Either String (BinaryTree Int)
demo06 = applyTransform (alltdR (add1 <+> idR)) tree1

demo06b :: Either String (BinaryTree Int)
demo06b = applyTransform (alltdR add1) tree1

demo07 :: Either String (BinaryTree Int)
demo07 = applyTransform (alltdR add1) tree2
    where
        tree2 = Bin 1 (Bin 2 Empty Empty) Empty
        
demo07b :: Either String (BinaryTree Int)
demo07b = applyTransform (alltdR (add1 <+> idR)) tree2
    where
        tree2 = Bin 1 (Bin 2 Empty Empty) Empty
        

demo07c :: Either String (BinaryTree Int)
demo07c = applyTransform (anytdR add1) tree2
    where
        tree2 = Bin 1 (Bin 2 Empty Empty) Empty        