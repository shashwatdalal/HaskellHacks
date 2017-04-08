import Data.List

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key table =  head[value | (key',value) <- table, key==key']

checkSat :: BDD -> Env -> Bool
checkSat (rootId,nodes) env = checkSat' (rootId,(lookUp rootId nodes))
  where
  checkSat' :: BDDNode -> Bool
  checkSat' (0,_) = False
  checkSat' (1,_) = True
  checkSat' (id,(envId,l,r))
    | lookUp envId env = checkSat' (r,(lookUp r nodes))
    | otherwise        = checkSat' (l,(lookUp l nodes))

sat :: BDD -> [[(Index, Bool)]]
sat (rootId,nodes) = sat' (rootId,(lookUp rootId nodes)) []
  where
  sat' :: BDDNode -> [(Index,Bool)] -> [[(Index,Bool)]]
  sat' (0,_) _   = []
  sat' (1,_) acc = [acc]
  sat' (id,(envId,l,r)) acc = 
    sat' (l,(lookUp l nodes)) ((envId,False):acc) ++ 
    sat' (r,(lookUp r nodes)) ((envId,True):acc)

------------------------------------------------------

simplify :: BExp -> BExp
simplify (Not (Prim b)) = Prim (not b)
simplify (And (Prim b1) (Prim b2)) = Prim (b1 && b2)
simplify (Or (Prim b1) (Prim b2))  = Prim (b1 || b2)
simplify x = x  

restrict :: BExp -> Index -> Bool -> BExp
restrict bexp@(IdRef i) i' bool
  |i' == i   = Prim bool
  |otherwise = bexp
restrict (Prim b) _ _       = Prim b
restrict (Not b) i bool     = simplify (Not (restrict b i bool))
restrict (And b1 b2) i bool = 
  simplify (And (restrict b1 i bool) (restrict b2 i bool))
restrict (Or b1 b2) i bool  =  
  simplify (Or (restrict b1 i bool) (restrict b2 i bool))
------------------------------------------------------

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = (2,(buildBDD' e 2 xs))

buildBDD' :: BExp -> NodeId -> [Index] -> [BDDNode]
buildBDD' e nI [x'] = [(nI,(x',nL,nR))]
  where  
  nL = if ((restrict e x' False) == Prim True) then 1 else 0
  nR = if ((restrict e x' True)  == Prim True) then 1 else 0
buildBDD' e nI (x':xs') =
 (buildBDD' (restrict e x' False) (2*nI) xs') ++
 ((nI,(x',2*nI,2*nI+1)) : 
 (buildBDD' (restrict e x' True) (2*nI + 1) xs'))   

------------------------------------------------------

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD 
  = undefined

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


