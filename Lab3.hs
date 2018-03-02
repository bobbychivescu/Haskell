import Tokens
import Grammar

--1,2
main = do
	content <- readFile "in.txt"
	let x = parseCalc $ alexScanTokens content
	putStr (show x)

--3
-- main = do
-- 	putStr ">"
-- 	line <- getLine
-- 	putStrLn $ show $ parseCalc $ alexScanTokens line
-- 	main

--5
type State = (Exp, Env, Kont)
data D = Clo (Exp, Env) | Int Int 
type Env = String -> D
data Kont = Mt
          | Ar (Exp,Env,Kont)
          | Fn (Exp,Env,Kont)

inject e = (e, p, Mt)
	where p = \x -> error $ "no binding for " ++ x

eval (Let a b c, p, k)