{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Demo03 where

import Language.KURE
import Text.JSON

import RoseTree

tree1 :: RoseTree Int
tree1 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]


demo0a :: IO ()
demo0a = putStrLn $ encode tree1

demo0b :: IO (RoseTree Int)
demo0b = do 
    json <- readFile "./demo/data/rose_tree1.json"
    let (ans::Result (RoseTree Int)) = decodeStrict json
    case ans of
        Error errMsg -> error errMsg
        Ok a1 -> return a1




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

positive :: RewriteA Int
positive =
    withPatFailExc (nodeMismatch "add1") $
          do Node a xs <- idR
             if a < 0 then return (Node (abs a) xs) else fail "already positive"
             

demo02 :: Either String (RoseTree Int)
demo02 = applyTransform add1 tree1             


demo03 :: Either String (RoseTree Int)
demo03 = applyTransform (oneR (add1 <+> idR)) tree1             

demo03b :: Either String (RoseTree Int)
demo03b = applyTransform (oneR add1) tree1

-- Left "failure" - no kids succeed
demo03c :: Either String (RoseTree Int)
demo03c = applyTransform (oneR positive) tree1

demo03d :: Either String (RoseTree Int)
demo03d = applyTransform (oneR (positive <+> idR)) tree1

-- note only (-10) is made positive
demo03e :: Either String (RoseTree Int)
demo03e = applyTransform (oneR positive) tree2
  where
    tree2 = Node (-1) [Node 5 [], Node (-10) [], Node 15 [], Node (-20) []]



demo04 :: Either String (RoseTree Int)
demo04 = applyTransform (anyR (add1 <+> idR)) tree1

demo04b :: Either String (RoseTree Int)
demo04b = applyTransform (anyR add1) tree1

demo04c :: Either String (RoseTree Int)
demo04c = applyTransform (anyR positive) tree1


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

-- in this case succeeds at the first node and doesn't need to descend into the kids.
demo08 ::  Either String (RoseTree Int)
demo08 = applyTransform (onetdR add1) tree1
        