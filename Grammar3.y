{ 
module Grammar3 where 
import Tokens3
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    f { TokenForward } 
    int { TokenInt $$ } 
    c { TokenCheck } 
    rt { TokenRotate } 
    r { TokenRight } 
    l { TokenLeft } 
    if { TokenIf } 
    else { TokenElse } 
    '(' { TokenLParen } 
    ')' { TokenRParen } 
    then { TokenThen }
    end {TokenEnd}


%% 

Exp : f int Exp                 { F $2 $3 }
    | if c int then '(' Exp ')' else '(' Exp ')' Exp         {If $3 $6 $10 $12}
    | rt l Exp                  {RtL $3}
    | rt r Exp                  {RtR $3}
    | end                       {End}
    
{ 
parseError :: [Token] -> a
parseError (x:xs) = error (show x) 
data Exp = F Int Exp
         | If Int Exp Exp Exp
         | RtL Exp
         | RtR Exp
         | End             
         deriving Show 
} 