module Calculus where

import Data.Maybe
import Data.List

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

unOpTable :: [(UnOp,(Double -> Double))]
unOpTable = [ (Neg,(0-))
            , (Sin,sin)
            , (Cos,cos)
            , (Log,log) ]

unOpStrTbl :: [(UnOp,String)]
unOpStrTbl = [ (Neg,"-")
             , (Sin,"sin")
             , (Cos,"cos")
             , (Log,"log") ]

binOpTable :: [(BinOp,(Double -> Double -> Double))]
binOpTable = [ (Add,(+))
             , (Mul,(*))
             , (Div,(/)) ]

binOpStrTbl :: [(BinOp,String)]
binOpStrTbl = [ (Add,"+")
             , (Mul,"*")
             , (Div,"/") ]

lookUp :: Eq a => a -> [(a, b)] -> b
loopUp _ [] = []
lookUp key ((k,v) : table) 
  | key == k  =  v
  | otherwise = lookUp key table

eval :: Exp -> Env -> Double
eval exp env = eval' exp
  where 
  eval' :: Exp -> Double
  eval' (BinApp bop x1 x2) = 
     (lookUp bop binOpTable) (eval x1 env) (eval x2 env)
  eval' (UnApp uop x1)     = (lookUp uop unOpTable) (eval x1 env) 
  eval' (Id s)             = (lookUp s env)
  eval' (Val d)            = d

diff :: Exp -> String -> Exp
diff (Val _) _ = Val 0
diff (Id s') s
  | s == s'   = Val 1
  | otherwise = Val 0
diff (BinApp binop e1 e2) s 
  | binop == Add = BinApp Add (diff e1 s) (diff e2 s)
  | binop == Mul = BinApp Add (BinApp Mul e1 (diff e2 s)) 
                              (BinApp Mul (diff e1 s) e2)
  | binop == Div = BinApp Div (BinApp Add (BinApp Mul (diff e1 s) e2)
                              (UnApp Neg (BinApp Mul e1 (diff e2 s))))
                              (BinApp Mul e2 e2)
diff (UnApp Neg e)  s = UnApp Neg (diff e s)
diff (UnApp unop e) s    
  | unop == Sin       = BinApp Mul (UnApp Cos e) (diff e s) 
  | unop == Cos       = UnApp Neg (BinApp Mul (UnApp Sin e) (diff e s) )
  | unop == Log       = BinApp Div (diff e s) e

taylor :: Exp -> Double -> Double -> Int -> Double
taylor e x a n = sum (zipWith (*) (zipWith (/) fas facts) as)
  where
  as    = 1 : iterate (*(x-a)) (x-a)
  facts = 1 : zipWith (*) facts [1..(fromIntegral (n-1))]
  fas   = map (flip eval [("x",a)]) (fxs e)
    where
    fxs :: Exp -> [Exp]
    fxs = iterate (flip diff "x")

maclaurin :: Exp -> Double -> Int -> Double
maclaurin e x = taylor e x 0

showExp :: Exp -> String
showExp (BinApp binop e1 e2) = "(" ++ (showExp e1) ++ (lookUp binop binOpStrTbl)
                                ++ (showExp e2) ++ ")"
showExp (UnApp uop e1)       = (lookUp uop unOpStrTbl) ++ "(" ++ (showExp e1) ++ ")"
showExp (Id s)               = s
showExp (Val v)              = show v

---------------------------------------------------------------------------

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

