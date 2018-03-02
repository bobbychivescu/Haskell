{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    let { TokenLet p } 
    in  { TokenIn p} 
    true   { TokenTrue p}
    false   { TokenFalse p}
    Integer   { TokenInteger p}
    Bool   { TokenBool p}
    int { TokenInt $$ p} 
    var { TokenVar $$ p} 
    '=' { TokenEq p} 
    '+' { TokenPlus p} 
    func { TokenFunc p} 
    if  { TokenIf p}
    else { TokenElse p}
    then { TokenThen p}
    ':' { TokenColon p} 
    '(' { TokenLParen p} 
    ')' { TokenRParen p} 
    '<' { TokenLt p}
    '\\' {TokenLambda p}

%right in 
%left '+' 
%% 

Exp : let Exp '=' Exp in Exp { Let $2 $4 $6 }
    | '(' var ':' Type ')'  {Tvar $2 $4} 
    | Exp '+' Exp            { Plus $1 $3 } 
    | Exp '<' Exp            { Lt $1 $3 } 
    | Exp Exp            { App $1 $2 } 
    | '\\' Exp Exp           { Lam $2 $3} 
    | '(' Exp ')'            { $2 } 
    | if Exp then Exp else Exp      { If $2 $4 $6} 
    | int                    { Int $1 } 
    | var                    { Var $1 } 
    | true			 { Tru}
    | false {Fals}
    

Type : Integer {Integer}
     | Bool {Bool}
     | Type func Type { F $1 $3}

{ 
parseError :: [Token] -> a
parseError (x:xs) = error (token_posn x) 

data Type = Integer | Bool | F Type Type deriving Show

data Exp = Let Exp Exp Exp 
         | Plus Exp Exp 
         | Lt Exp Exp 
         | App Exp Exp 
         | Lam Exp Exp
         | Tvar String Type 
         | If Exp Exp Exp
         | Int Int 
         | Var String 
         | Tru 
         | Fals
         deriving Show 
} 