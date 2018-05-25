import Tokens
import Grammar
import Data.List
import Data.String
import System.Environment
import Control.Applicative

main = do
	arguments <- getArgs
	case arguments of 
		[file] -> do
			content <- readFile (checkFile file)
			--content <- readFile "in.cql"
			--let x = parseIt $ alexScanTokens content
			--putStrLn $ show x
			interpretComSeq $ parseIt $ alexScanTokens content
			
		_ -> putStrLn "Wrong number of arguments"	
--			 T  header    content
data Table = T [String] [[String]]

checkFile:: String -> String
checkFile fileName 
	| drop ((length fileName)-4) fileName == ".cql" = fileName
	| otherwise = error "Error, wrong extension"

--interpretComSeq :: CSQ -> IO ()
interpretComSeq (CS com1 com2) = do
	 rez1 <- interpret com1
	 put rez1
	 interpretComSeq com2
	 


interpretComSeq (CE com) = do
	rez <- interpret com
	put rez

--interpret :: Com -> IO ()
interpret (C seq exp) = do  
	let seq' = interpretSeq seq -- [String]
	--let c = checkLam seq' exp
  	tables <- rde (checkExpression seq' exp)
  	let exp' = interpretExp exp tables-- table
  	return (solve seq' exp')


checkExpression :: [String] -> Exp -> Exp 
checkExpression seq (And exp1 exp2) = (And (checkExpression seq exp1) (checkExpression seq exp2))

checkExpression seq (Eql (V var1) (V var2)) 
	| (checkVaraible var1 seq) == False = error("Variable not decalred (in equality): " ++ var1)
	| (checkVaraible var2 seq) == False = error("Variable not decalred (in equality): " ++ var2)
	| otherwise = (Eql (V var1) (V var2)) 

checkExpression seq (Lam (V var) exp) 
	| elem var seq = error ("Name clash on bound variable (with free variables or previous bound variables): " ++ var) 
	| otherwise = (Lam (V var) (checkExpression (var : seq) exp))

checkExpression seq (Qry rel seq1) 
	| fst rez == False  = error ("Variable not declared (in relation): " ++ snd rez)
	| otherwise =  (Qry rel seq1)
	where 
		rez = checkVaraibles (interpretSeq seq1) seq

--reorder table according to seq and sort entries
--returns [String] - sorted
--error - check if seq and header are same
solve seq (T header entry) 
	| fst rez == False = error ("Unused free variable  (left side): " ++ snd rez)
	| otherwise =  let ordered = map (reorder seq header) entry
				   in sort [intercalate "," x | x<-ordered]
	where
		rez =  checkVaraibles seq header 

checkVaraible:: String -> [String] -> Bool
checkVaraible _ [] = False
checkVaraible toFind (header:list) 
	| toFind == header = True
	| otherwise = checkVaraible toFind list

checkVaraibles :: [String] -> [String] -> (Bool, String)
checkVaraibles [] _  = (True,[])
checkVaraibles (x:xs) y
	| checkVaraible x y == True = checkVaraibles xs y
    | otherwise = (False, x)


reorder [] _ _ = []
reorder (x:xs) header singleEntry = 
	singleEntry!!(pos x header) : reorder xs header singleEntry

put [] = return ()
put (x:xs) = do
	putStrLn x
	put xs

interpretSeq :: Seq -> [String]
interpretSeq (S (V var)) = [var];
interpretSeq (M (V var) seq) = var : interpretSeq seq

interpretExp :: Exp -> [String] -> Table
interpretExp exp db= table1 where
	(_, table1) = (app (interpretExpM exp db)) (T [] [[]])

--nu scapam fara monad
--regula in limbaj: o variablila trebuie citita inainte sa fie folosita
--e.g. R(x1) & Q(x2) & x1=x2		ok
--	   R(x1) & x1=x2 & Q(x2)		not

newtype M a = Build (Table -> (a, Table))

instance Functor M where
	fmap f soFar = Build g where
		g table = (f x, newTable) where
			(x, newTable) = (app soFar) table

instance Applicative M where
	pure x = Build f where
		f table = (x, table)

	soFar <*> next = Build f where
		f table = (g x, table2) where
			(g, table1) = (app soFar) table
			(x, table2) = (app next) table1


instance Monad M where
	return x  = Build f where
		f table = (x, table)

	soFar >>= f = Build g where
		g table = (app (f stuff)) table1 
			where
				(stuff, table1) = (app soFar) table

app (Build f) = f;


interpretExpM :: Exp -> [String] -> M [String]
interpretExpM (And exp1 exp2) db = 
	do
		db1 <- interpretExpM exp1 db
		db2 <- interpretExpM exp2 db1
		return db2

interpretExpM (Eql (V v1) (V v2)) db = 
	do
		filterEqual v1 v2 --o sa se uite in tabelul curent si pastreaza doar entry bune
		return db

interpretExpM (Lam (V v) exp) db = 
	do
		interpretExpM exp db
		extract v 
		return []


interpretExpM (Qry file seq) (e:db) =
	do
		let table = [split x ',' | x <- lines e] --will have to be tested
		--assure that seq.len == each of table.len
		construct (interpretSeq seq) table file
 		return db


posExtract x y
	| rez == length y = error ("Bound variable not used: " ++ x)
	| otherwise = rez
	where rez = pos x y

posHeader x y 
	| rez == length y = error ("Variable not used in any previous relation: " ++ x)
	| otherwise = rez
	where rez = pos x y

pos x [] = 0--error 
pos x (y:ys) = if x==y then 0 else 1+pos x ys

filterEqual :: String -> String -> M ()
filterEqual v1 v2 = Build f where
	f (T header entry) = ((), T header newEntry)
		where
			newEntry = [x | x<-entry, x!!(posHeader v1 header) == x!!(posHeader v2 header)]
			
extract :: String -> M ()
extract v = Build f where
	f (T header entry) = ((), table2) -- remove from table and merge
		where
			table2 = let p = posExtract v header in
				T (delete v header) [take p x ++ drop (p+1) x | x<-entry]

merge (T h1 e1) (T h2 e2) = T h3 e3
	where
		h3 = h1 ++ [x | x<-h2, not $ elem x h1]
		common = [x | x<-h2, elem x h1]
		pos1 = map (\s -> pos s h1) common
		pos2 = map (\s -> pos s h2) common
		e3 = [ x++ removeDuplicate y pos2| x<-e1 , y<-e2, areEq x y]
		areEq x y = check x pos1 y pos2
		check _ [] _ [] = True
		check x (p1:p1s) y (p2:p2s) = if x!!p1==y!!p2 
										then check x p1s y p2s
										else False
		removeDuplicate xs [] = xs										
		removeDuplicate xs (y:ys) = removeDuplicate (delete (xs!!y) xs) (map (+(-1)) ys)

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

--must retrun IO [string]
rde x = do
	rez <- rd x
	if rez==[]  then error "No relations declared!" else return rez

rd (And e1 e2) = do
	t1 <- rd e1
	t2 <- rd e2
	return (t1++t2)
rd (Eql _ _) = return []
rd (Lam _ e) = rd e
rd (Qry file _) = do
	content <- readFile (file++".csv")
	return [content]

checkLenOfAllRows :: [[String]] -> (Bool, Int)
checkLenOfAllRows [] = (True, 0)
checkLenOfAllRows (x:xs) 
	| length xs == 0 = (True, length x)
	| fst nextIt && length x == snd nextIt = (True, snd nextIt)
	| otherwise = (False, snd nextIt )
	where 
		nextIt = checkLenOfAllRows xs

construct :: [String] -> [[String]] -> String -> M ()
construct header entry file
	| constantLen == True && (seqLen == tableLen || tableLen == 0) = Build f
	| otherwise = error ("Incorrect number of columns and in relation " ++ file ++ " .\nExpecting " ++ show seqLen ++ " columns and encoutered " ++ show tableLen)
	where
		rez1 = checkLenOfAllRows entry
		constantLen = fst rez1
		tableLen = snd rez1
		seqLen = length header
		f table = ((), merge table (T header entry))
