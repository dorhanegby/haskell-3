-- Dor Hanegby
-- 204009633

data Expr = Const Value
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Var Variable

type Variable = String
type Value = Float

type Dictionary = [(Variable, Value)]
type EvalError = [Variable]
type EvalResult = Either EvalError Value

-- 1.
-- a.

display :: Expr -> String
display (Var var) = var
display (Const val) = show val
display (Add a b) = "(" ++ display a ++ "+" ++ display b ++ ")"
display (Mul a b) = "(" ++ display a ++ "*" ++ display b ++ ")"
display (Sub a b) = "(" ++ display a ++ "-" ++ display b ++ ")"
display (Div a b) = "(" ++ display a ++ "/" ++ display b ++ ")"

-- b.

filterListWithValue :: Dictionary -> Expr -> Dictionary
filterListWithValue dic (Var var) = filter(\(char, num) -> char == var) dic

eval :: Dictionary -> Expr -> EvalResult
eval dic expr = let (val, error) = eval' dic expr [] in
    if error == [] then Right (val)
    else Left (error)

eval' :: Dictionary -> Expr -> EvalError -> (Value, EvalError)
eval' _ (Const val) list = (val, list)
eval' dic (Var a) list = let filteredList = filterListWithValue dic (Var a) in
    if filteredList == [] then  (0 ,(a : list))
    else eval' dic (Const (snd(head(filteredList)))) list
eval' dic (Add a b) list = let (a', errA) = eval' dic a list; (b', errB) = eval' dic b list  in (a' + b', errA ++ errB)
eval' dic (Mul a b) list = let (a', errA) = eval' dic a list; (b', errB) = eval' dic b list  in (a' * b', errA ++ errB)
eval' dic (Sub a b) list = let (a', errA) = eval' dic a list; (b', errB) = eval' dic b list  in (a' - b', errA ++ errB)
eval' dic (Div a b) list = let (a', errA) = eval' dic a list; (b', errB) = eval' dic b list  in (a' / b', errA ++ errB)

-- 2.

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b) deriving Show

-- a.

reverseTree :: Tree a b -> Tree a b
reverseTree (Leaf l) = (Leaf l)
reverseTree (Node a l r) = Node a (reverseTree r) (reverseTree l)

-- b.

isEqualTree :: Tree Int Char -> Tree Int Char -> Bool
isEqualTree (Leaf a) (Leaf b) = a == b
isEqualTree (Node a l r) (Leaf b) = False
isEqualTree (Leaf b) (Node a l r) = False
isEqualTree (Node a l1 r1) (Node b l2 r2) = a == b && isEqualTree l1 l2 && isEqualTree r1 r2

isSubtree :: Tree Int Char -> Tree Int Char -> Bool
isSubtree (Leaf a) (Leaf b) = isEqualTree (Leaf a) (Leaf b)
isSubtree (Node a l r) (Leaf b) = False
isSubtree (Leaf b) (Node a l r) = isSubtree (Leaf b) l || isSubtree (Leaf b) r
isSubtree (Node a l1 r1) (Node b l2 r2) = if isEqualTree (Node a l1 r1) (Node b l2 r2) then True else isEqualTree (Node a l1 r1) l2 || isEqualTree (Node a l1 r1) r2

-- 3.

data MTree a = MTree a [MTree a] deriving Show

-- a.

sumMTree :: MTree Int -> Int
sumMTree (MTree val []) = val
sumMTree (MTree val children) = val + sum (map (sumMTree) children)

-- b.

grow :: MTree a -> MTree a
grow (MTree val []) = MTree val []
grow (MTree val children) = MTree val (map (grow' (MTree val children)) children)

grow' :: MTree a -> MTree a -> MTree a
grow' origin (MTree val []) = origin
grow' origin (MTree val children) = MTree val (map (grow' (origin)) children)

