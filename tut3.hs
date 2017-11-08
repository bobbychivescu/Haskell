
--ex1
-- sum ([x^2 | x <- [1, 3..99]] ++ [x^3 | x<-[2, 4..100]])

--ex2
grid :: Int -> Int -> [(Int,Int)] 
grid x y = [(a, b) | a<-[0..x], b<-[0..y]]

square :: Int -> [(Int,Int)]
square x  = [(a,b) | a<-[0..x], b<-[0..x], a/=b]

--ex3
repl :: Int -> a -> [a]
repl 1 a = [a]
repl x a = [a] ++ (repl (x-1) a)

--ex4
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a, b, c) | a<-[0..x], b<-[0..x], c<-[0..x], a^2+b^2==c^2]

--ex5
fac :: Int -> [Int]
fac x = [y | y<-[1..x `div` 2], x `mod` y == 0]

perfect :: Int -> [Int]
perfect x = [a | a<-[1..x], a == (sum (fac a))]

--ex6
find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

--ex7
sp :: [Int] -> [Int] -> Int
sp [] [] = 0
sp (x:xs) (y:ys) = x*y + sp xs ys

--ex8
sumdown :: Int -> Int -> Int
sumdown x y | y>=0 		= sum [y..x]
			| otherwise = sum [0..x]

--ex9
euclid :: Int -> Int -> Int
euclid x y 	| x==y = x
			| x>y = euclid (x-y) y
			| y>x = euclid x (y-x)

--ex10
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if x>y 	then y:(merge (x:xs) ys)
								else x:(merge xs (y:ys))
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs = merge (mergeSort (take j xs)) (mergeSort (drop j xs))
	where j = (length xs) `div` 2
