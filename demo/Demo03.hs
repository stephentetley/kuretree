{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Demo03 where

import Language.KURE

import RoseTree

tree1 :: RoseTree Int
tree1 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]


-- | For this simple example, the context is just an 'AbsolutePath', and transformations always operates on 'Arith'.
type TransformA a b = Transform () KureM (RoseTree a) b
type RewriteA a = TransformA a (RoseTree a)


applyTransform :: TransformA a b -> (RoseTree a) -> Either String b
applyTransform t = runKureM Right (Left . showKureExc) . applyT t mempty

demo01 :: Either String (RoseTree Int)
demo01 = applyTransform idR tree1

add1 :: RewriteA Int
add1 = withPatFailExc (nodeMismatch "add1") $
          do Node a xs <- idR
             return (Node (a + 1) xs)
             

demo02 :: Either String (RoseTree Int)
demo02 = applyTransform add1 tree1             


demo03 :: Either String (RoseTree Int)
demo03 = applyTransform (oneR (add1 <+> idR)) tree1             

demo03b :: Either String (RoseTree Int)
demo03b = applyTransform (oneR add1) tree1

demo04 :: Either String (RoseTree Int)
demo04 = applyTransform (anyR (add1 <+> idR)) tree1

demo04b :: Either String (RoseTree Int)
demo04b = applyTransform (anyR add1) tree1

demo05 :: Either String (RoseTree Int)
demo05 = applyTransform (allR (add1 <+> idR)) tree1

demo05b :: Either String (RoseTree Int)
demo05b = applyTransform (allR add1) tree1


demo06 :: Either String (RoseTree Int)
demo06 = applyTransform (alltdR (add1 <+> idR)) tree1

demo06b :: Either String (RoseTree Int)
demo06b = applyTransform (alltdR add1) tree1

demo07 :: Either String (RoseTree Int)
demo07 = applyTransform (alltdR add1) tree2
    where
        tree2 = Node 1 [Node 2 []]
        
demo07b :: Either String (RoseTree Int)
demo07b = applyTransform (alltdR (add1 <+> idR)) tree2
    where
        tree2 = Node 1 [Node 2 []]
        

demo07c :: Either String (RoseTree Int)
demo07c = applyTransform (anytdR add1) tree2
    where
        tree2 = Node 1 [Node 2 []]
        