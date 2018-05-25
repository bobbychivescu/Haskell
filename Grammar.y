{ 
module Grammar where 
import Tokens 
}

%name parseIt 
%tokentype { Token } 
%error { parseError }
%token 
    ',' { TokenComma p}
    '&' { TokenAnd p}
    '.' { TokenDot p}
    var { TokenVar $$ p} 
    rel { TokenRel $$ p}
    '=' { TokenEq p} 
    ':' { TokenColon p} 
    '(' { TokenLParen p} 
    ')' { TokenRParen p} 
    '\\' {TokenExist p}
    ';'  {TokenSemi p}

%right '.'
%right '&'
%% 


CSQ : Com CSQ {CS $1 $2}
    | Com {CE $1 }

Com : Seq ':' Exp ';' {C $1 $3}

Exp : Exp '&' Exp {And $1 $3}
    | Var '=' Var {Eql $1 $3}
    | '\\' Var '.' Exp {Lam $2 $4}
    | rel '(' Seq ')' {Qry $1 $3}

Seq : Var ',' Seq {M $1 $3}
    | Var {S $1}

Var : var {V $1}

{ 
parseError :: [Token] -> a
parseError [] = error "Expected ; "
parseError (x:xs) = error (token_posn x) 
                     

data CSQ = CS Com CSQ
         | CE Com
         deriving Show

data Com = C Seq Exp 
         deriving Show

data Exp = And Exp Exp
         | Eql Var Var
         | Lam Var Exp
         | Qry String Seq 
         deriving Show 

data Seq = M Var Seq
         |S Var 
         deriving Show

data Var = V String 
         deriving (Eq, Show)
} 