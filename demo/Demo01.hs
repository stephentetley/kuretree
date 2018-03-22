{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}


module Demo01 where

import KureTree

import Text.JSON.String

demo01 = do 
    json <- readFile "./demo/data/tree1.json"
    let (ans::Either String [Row]) = runGetJSON readJsonFlat json
    putStrLn $ show $ fmap length ans
    return $ ans