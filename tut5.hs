--ex1
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = if compare a b == EQ then True else False
occurs a (Node b c d) = if com == EQ then True else
	if com == LT then occurs a b else occurs a d
	where com = compare a c

--ex2
fldl :: (a -> a -> a) -> a -> Tree a -> a
fldl f b (Leaf a) = f b a
fldl f b (Node l a r) = fldl f (f lt a) r
	where lt = fldl f b l

--ex3

--ex4
data Prop = Const Bool | Var Char | Not Prop 
          | And Prop Prop | Imply Prop Prop

data Tip = Negative | Pozitive | Mixed | Nada
	deriving (Show)
(<>) :: Tip -> Tip -> Tip
Negative <> Negative = Negative
Negative <> Pozitive = Mixed
Negative <> Mixed = Mixed
Negative <> Nada = Negative
Pozitive <> Negative = Mixed
Pozitive <> Pozitive = Pozitive
Pozitive <> Mixed = Mixed
Pozitive <> Nada = Pozitive
Mixed <> _ = Mixed
Nada <> x = x


getForm :: Prop -> Tip
getForm xs = ev xs 0
	where
		ev (Const _) _ = Nada
		ev (Var _) x = if even x then Pozitive else Negative
		ev (Not p) x = ev p (x+1)
		ev (And p q) x = ev p x <> ev q x
		ev (Imply p q) x = ev p (x+1) <> ev q x

--ex5
data Pair a b = P (a, b)
	deriving Show

instance Functor (Pair a) where
	fmap f (P (a, b)) = P (a, f b)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
