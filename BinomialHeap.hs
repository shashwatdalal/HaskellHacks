type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------

value :: BinTree a -> a
value (Node v _ _) = v 

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1@(Node v1 r1 c1) t2@(Node v2 r2 c2)
  | v1<v2  = Node v1 (r1+1) (t2:c1)
  | v2<=v1 = Node v2 (r1+1) (t1:c2)

--------------------------------------------------------------

extractMin :: Ord a => BinHeap a -> a
extractMin = (value.minimum)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] [] = []
mergeHeaps h1 [] = h1
mergeHeaps [] h2 = h2
mergeHeaps h1@(h1':h1s') h2@(h2':h2s')
  | rank h1' < rank h2' = h1' : mergeHeaps h1s' h2
  | rank h2' < rank h1' = h2' : mergeHeaps h2s' h1
  | otherwise 
    = mergeHeaps [combineTrees h1' h2'] (mergeHeaps h1s' h2s')  

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert x = mergeHeaps [Node x 0 []]

removeMin :: Ord a => BinHeap a -> BinHeap a
removeMin hs
   = mergeHeaps (filter (/=minTree) hs) (reverse c)
  where
   minTree@(Node _ _ c) = minimum hs

binSort :: Ord a => [a] -> [a]
binSort xs =  take (length xs) (map (extractMin) (iterate removeMin bH))
  where 
    bH = (foldl1 mergeHeaps [[Node x 0 []] | x<-xs])
--------------------------------------------------------------

binarySum :: [Int] -> [Int] -> [Int]
binarySum
  = undefined

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



