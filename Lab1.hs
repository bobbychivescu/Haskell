

--1
zipL :: ([a], [a]) -> [[a]]
zipL ([],[]) = []
zipL ((x:xs), (y:ys)) = if length xs == length ys
then [x,y]:zipL (xs, ys)
else []

unzipL s = f ([],[]) s
	where
		f x [] = x
		f (a, b) ([x,y]:ss) = f (a++[x], b++[y]) ss

--2
zip2L :: ([a], [a]) -> [[a]]
zip2L ([],[]) = []
zip2L ([], (x:xs)) = [x]:zip2L([], xs)
zip2L ((x:xs), []) = [x]:zip2L(xs, [])
zip2L ((x:xs), (y:ys)) = [x,y]:zip2L (xs, ys)

--3
multiZipL :: [[a]] -> [[a]]
multiZipL xs = foldl f [] xs
	where
		f [] [] = []
		f [] (y:ys) = [y]:f [] ys
		f xs [] = xs
		f (x:xs) (y:ys) = (x++[y]):f xs ys

--4
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

toS a = read a :: Int

multizipF :: String -> IO ()
multizipF path = do
	content <- readFile path
	let l = lines content
	let c = [map toS $ split (l!!n) ',' | n<-[0..length l-1]]
	writeFile "out.txt" $ show $ multiZipL c
	