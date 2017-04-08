data Tree a = Leaf a | Node (a->Bool) (Tree a) (Tree a) 

formTree :: Eq a => [a] -> [Int] -> Tree a
--create a tree with respective 'questions'
--pre: probability and respective x element sorted in decending order
--     atleast one element and respective probabilty 
formTree [x] _ = Leaf x
formTree x@(x1:x2:xs) p@(p1:p2:ps)
  | allEqual p = Node (\y -> or (map (==y) xL)) (formTree xL pL) (formTree xR pR)  
  | otherwise  = Node (==x1) (Leaf x1) (formTree (x2:xs) (p2:ps))
  where 
    (xL,xR) = splitAt ((length x) `div` 2) x
    (pL,pR) = splitAt ((length p) `div` 2) p

allEqual :: Eq a => [a] -> Bool
allEqual xs = and (zipWith (==) xs (tail xs))

showTree :: (Show a) => Tree a -> String
showTree (Leaf x) = show x
showTree (Node _ t1 t2) = show t1 ++ show t2
