import Data.Char
import Data.List

--ex1
subtotal :: Num a => [a] -> [a]
subtotal [] = []
subtotal (x:xs) = x:[x+i | i<-subtotal xs]

--ex2
histogram :: Int -> [Int] -> [Int]
histogram 0 xs = []
histogram x [] = []
histogram x xs = [length [m | m<-xs, m<z+x, m>=z] | z<-[0, x..maximum xs]]

--ex3
meetsOffer :: String -> Int -> Bool
meetsOffer "" x = if x<=0 then True else False
meetsOffer (c:s) x 	| c=='A' = meetsOffer s (x-48)
					| c=='B' = meetsOffer s (x-40)
					| c=='C' = meetsOffer s (x-32)
					| c=='D' = meetsOffer s (x-24)
					| c=='E' = meetsOffer s (x-16)
					| c=='*' = meetsOffer s (x-8)

--ex4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted 
	deriving Show

sortType :: (Ord a)=> [a] -> TypeOfSort
sortType [] = NotSorted
sortType [x] = NotSorted
sortType xs	| isA xs	=Ascending
			| isC xs	=Constant
			| isND xs	=NonDescending
			| isD xs	=Descending
			| isNA xs	=NonAscending
			| otherwise =NotSorted
	where
		isA [x] = True
		isA (x:y:xs) = x<y && isA (y:xs)
		isC [x] = True
		isC (x:y:xs) = x==y && isC (y:xs)
		isND [x] = True
		isND (x:y:xs) = x<=y && isND (y:xs)
		isD [x] = True
		isD (x:y:xs) = x>y && isD (y:xs)
		isNA [x] = True
		isNA (x:y:xs) = x>=y && isNA (y:xs)


--ex5
rpcalc :: String -> Int
rpcalc "" = 0
rpcalc (x:y:xs) = head (f [digitToInt y, digitToInt x] xs)
	where
		f stack [] = stack 
		f (a:b:stack) (x:xs) 	| x=='+' 	= f ((a+b):stack) xs
								| x=='*' 	= f ((a*b):stack) xs
								| x=='-' 	= f ((b-a):stack) xs
								| x=='/' 	= f ((b `div` a):stack) xs
								| otherwise =f (digitToInt x:a:b:stack) xs

--ex6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs = [snd m | m<-take k sorted]
	where
		dist c b = sqrt $ (fst c - fst b)^2 + (snd c - snd b)^2
		tuple = [(dist p m, m) | m<-xs]
		cmp a b | fst a > fst b = GT
				| fst a == fst b = EQ
				| otherwise = LT
		sorted = sortBy (cmp) tuple

--ex7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int
	deriving Show
balanced :: SearchTree -> Bool
balanced (Leaf _) = True
balanced (Node x y z) = if val x<y && y<val z &&
							abs (dif x z)<=1 &&
							balanced x && balanced z then True else False
	where 
		val (Leaf x) = x
		val (Node a b c) = b
		height (Leaf _) = 1
		height (Node x y z) = (max (height x) (height z)) + 1
		dif x y = height x - height y  

--ex8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = 1 : next 1 d
	where
		next i d = x : next x d
			where
				x = (i+d/i)/2 

newtonRoot :: Double -> Double -> Double
newtonRoot d ep = get ep (newtonRootSequence d)
	where
		get ep (x:y:xs) = if abs (y-x) < ep then y else get ep (y:xs)

--ex9
hyperOperator :: Int -> Int -> Int -> Int
hyperOperator 1 a b = a+b
--the 2nd and 3rd operators are defined only for faster execution,
--they could easily be matched by the recursive definition
hyperOperator 2 a b = a*b
hyperOperator 3 a b = a^b
hyperOperator n a b = if b==0 then 1 else
	hyperOperator (n-1) a (hyperOperator n a (b-1))

--ex10
encode :: String -> [Int]
encode "" = []
encode (x:xs) = addedBit ++ encode xs
	where
		enc 1 = [1]
		enc x = x `mod` 2 : enc (x `div` 2)
		a = enc $ ord x
		bin = take (8 - (length a)) (repeat 0) ++ reverse a
		addedBit = bin ++ [if odd $ length (filter (==1) bin) then 1 else 0]

--ex11
decode :: [Int] -> String
decode [] = []
decode xs = if length xs `mod` 9==0 && test xs then 
	[chr $ foldr f 0 (zip byte [7,6..0])] ++ decode (drop 9 xs)
 	else ""
	where
		test [] = True
		test xs = if odd $ length (filter (==1) (take 9 xs)) then False else
			test (drop 9 xs)
  		byte = take 8 $ take 9 xs
  		f a b = fst a * 2^snd a + b

--ex12
makeChange :: (Num a, Eq a, Ord a) => a -> [a] -> [Int]
makeChange 0 xs = take (length xs) (repeat 0)
makeChange m xs = if m>0 then choose $ map f xs else take (length xs) (repeat (-1))
	where
		choose :: [[Int]] -> [Int]
		choose xs = minimumBy (cmp) xs
		cmp x y | pozSum x > pozSum y = GT
				| pozSum x == pozSum y = EQ
				| otherwise = LT
		--workaround the no solution case
		pozSum xs = if sum xs>=0 then sum xs else maxBound :: Int
		f x = incr (poz x xs) (makeChange (m-x) xs)
		poz y (x:xs) = if x==y then 0 else 1+poz y xs
		--dont increse lists of -1
		incr n xs = if length xs == length (filter (==(-1)) xs) then xs else
				take (n) xs ++ [xs!!n+1] ++ drop (n+1) xs 

--ex13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (b, xs) = if maximum xs >= b then [] else
 	(b, xs) : next 
 	where
 		num = sum [fst m*(b+1)^snd m | m<-zip xs [0..length xs]]-1
 		toBaseB x = if x==0 then [] else (x `mod` (b+1)) : (toBaseB $ x `div` (b+1))
 		elem = (b+1, toBaseB num)
 		next = if num==0 then [elem] else (goodsteinSequence elem)

--ex14
--all functions in this exercise are taken
--from the book, except for isSat

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

type Assoc k v = [(k,v)]
fnd :: Eq k => k -> Assoc k v -> v
fnd k t = head [v | (k1,v)<-t , k==k1]

type Subst = Assoc Char Bool
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x ) = fnd x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char ]
vars (Const _) = [ ]
vars (Var x ) = [x ]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [ [Bool ] ]
bools 0 = [[ ]]
bools n = map (False:) bss ++ map (True:) bss
	where bss = bools (n-1)

rmdups :: Eq a => [a ] -> [a ]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
	where vs = rmdups (vars p)

isSat :: Prop -> Subst
isSat p = firstTrue [(eval s p ,s) | s <- substs p]
	where 
		firstTrue [] = []
		firstTrue (x:xs) = if fst x then snd x else firstTrue xs

--ex15
isCantorPair :: Int -> Bool
isCantorPair z = if xp+yp==y then True else False
	where
		roots z = ((w-z+t), (z-t))
			where
				w = floor ((sqrt (fromIntegral (8*z)+1)-1)/2)
				t = (w^2+w) `div` 2
		x = fst $ roots z
		y = snd $ roots z
		xp = fst $ roots x
		yp = snd $ roots x