import Data.Char

--ex9
enc :: Int -> String -> String
enc i "" = ""
enc i (x:xs) = [chr ((ord x)+i)] ++ enc i xs 

dec :: String -> Int -> (String, (String ->String))
dec xs i = (enc (-i) xs, enc i)

--ex10
d :: Int -> Int
d x = if x*2>9 then x*2-9 else x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c e = (d a + b + d c + e) `mod` 10 == 0