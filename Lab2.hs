import Tokens3
import Grammar3

main = do
	content <- readFile "in.txt"
	let x = parseCalc $ alexScanTokens content
	putStr (show x)