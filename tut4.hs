import Data.List

--ex1
all :: (a -> Bool) -> [a] -> Bool
all f xs = foldr (&&) True (map f xs)

any :: (a -> Bool) -> [a] -> Bool
any f xs = foldr (||) False (map f xs)

takWhile :: (a -> Bool) -> [a] -> [a]
takWhile f [] = []
takWhile f (x:xs) = if f x then x:(takWhile f xs) else [] 

drpWhile :: (a -> Bool) -> [a] -> [a]
drpWhile f [] = []
drpWhile f (x:xs) = if f x then drpWhile f xs else x:xs

--ex2
dec2Int :: [Int] -> Int
dec2Int (x:xs) = foldl (f) x xs 
	where
		f a b = 10*a+b

--ex3


--ex4
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin a = reverse $ unfold p h t a
	where
		p x = if x==0 then True else False
		h x = x `mod` 2
		t x = x `div` 2 

chop s l = unfold p (take l) (drop l) s
	where 
		p x = if length x<l then True else False

iterate f x = unfold p h f x
	where
		p x = False
		h x = x

--ex5
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

--ex6
luhn xs = if (sum $ altMap dub unit xs) `mod` 10 == 0 then True else False
	where 
		dub x = if x*2>9 then x*2-9 else x*2
		unit x = x

--ex7
-- data Nat = Zero | Succ Nat
--    deriving (Eq,Ord,Show,Read)

-- evn :: Nat -> Bool
-- evn Zero = True
-- evn (Succ x) = not $ evn x

-- od :: Nat -> Bool
-- od x = not $ evn x

-- add :: Nat -> Nat -> Nat
-- add Zero x = x
-- add (Succ x) y = Succ (add x y)

-- mult :: Nat -> Nat -> Nat
-- mult Zero x = Zero
-- mult x Zero = Zero
-- mult (Succ x) y = add y (mult x y)

--ex8
data RInt = Zero | Succ RInt | Pred RInt
	deriving (Eq,Ord,Show,Read)

normalise :: RInt -> RInt
normalise Zero = Zero
normalise x = if c>=0 then plus c
						else minus (-c)
	where 
		plus 0 = Zero
		plus x = Succ $ plus (x-1)
		minus 0 = Zero
		minus x = Pred $ minus (x-1)
		cnt Zero = 0
		cnt (Succ x) = 1+cnt x
		cnt (Pred x) = cnt x-1
		c=cnt x

--ex9

--ex10
data Tree a = Leaf a | Node (Tree a) a (Tree a) | EmptyLeaf
	deriving Show

toTree :: Ord a => [a] -> Tree a
toTree [] = EmptyLeaf
toTree [x] = Leaf x
toTree xs = if and $ zipWith (<=) xs (tail xs) then
	Node (toTree $ take l xs) (xs!!l) (toTree $ drop (l+1) xs)
	else toTree $ sort xs
	where
		l=length xs `div` 2