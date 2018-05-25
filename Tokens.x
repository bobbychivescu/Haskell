{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9 
$lowerCase = [a-z]
$upperCase = [A-Z] 

tokens :-
  $white+       ; 
  "#".*         ; 
  \,            { \p s -> TokenComma p}
  \:            { \p s -> TokenColon p}
  \(            { \p s -> TokenLParen p}
  \)            { \p s -> TokenRParen p}
  \&            { \p s -> TokenAnd p}
  \=            { \p s -> TokenEq p}
  \\            { \p s -> TokenExist p}
  \.            { \p s -> TokenDot p}
  \;            { \p s -> TokenSemi p}
  $lowerCase [$lowerCase $digit $upperCase \_ \’]*   { \p s -> TokenVar s p}
  $upperCase [$lowerCase $digit $upperCase \_ \’]*   { \p s -> TokenRel s p}

{ 

--putem sa adaugam mai maic, mai mare, sau...

data Token = 
  TokenComma AlexPosn       | 
  TokenColon AlexPosn       | 
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenAnd AlexPosn         |
  TokenEq AlexPosn          |
  TokenExist AlexPosn       |
  TokenDot AlexPosn         |
  TokenVar String AlexPosn  |
  TokenRel String AlexPosn  |
  TokenSemi AlexPosn
  deriving (Eq,Show) 


token_posn (TokenComma  (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenColon  (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenLParen (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenRParen (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenAnd    (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenEq     (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenExist  (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenDot    (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenVar _  (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenRel _  (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b
token_posn (TokenSemi   (AlexPn _ a b)) ="Line: " ++ show a ++ ", Column: " ++ show b

}

