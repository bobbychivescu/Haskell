{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  "--".*        ; 
  let           { \p s -> TokenLet p} 
  in            { \p s -> TokenIn p}
  true          { \p s -> TokenTrue p}
  false         { \p s -> TokenFalse p}
  Int           { \p s -> TokenInteger p}
  Bool          { \p s -> TokenBool p}
  $digit+       { \p s -> TokenInt (read s) p} 
  \=          { \p s -> TokenEq p}
  \+          { \p s -> TokenPlus p}
  \-\>          { \p s -> TokenFunc p}
  if          { \p s -> TokenIf p}
  else          { \p s -> TokenElse p}
  then          { \p s -> TokenThen p}
  \:          { \p s -> TokenColon p}
  \(          { \p s -> TokenLParen p}
  \)          { \p s -> TokenRParen p}
  \<		  { \p s -> TokenLt p}
  \\      { \p s -> TokenLambda p}
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar s p} 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenLet AlexPosn        | 
  TokenIn AlexPosn          | 
  TokenInt Int AlexPosn     |
  TokenVar String AlexPosn  |
  TokenInteger AlexPosn     | 
  TokenEq AlexPosn          |
  TokenTrue AlexPosn          |
  TokenBool AlexPosn      |
  TokenFalse AlexPosn          |
  TokenPlus AlexPosn        |
  TokenFunc AlexPosn       |
  TokenIf AlexPosn       |
  TokenElse AlexPosn       |
  TokenThen AlexPosn       |
  TokenColon AlexPosn         |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenLt AlexPosn |
  TokenLambda AlexPosn
  deriving (Eq,Show) 


token_posn (TokenLet (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn (TokenIn (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn (TokenInt _ (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn (TokenVar _ (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn (TokenEq (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenPlus (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenIf (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenFunc (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenColon (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenLParen (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenRParen (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenLt (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenTrue (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenFalse (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenInteger (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenBool (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenElse (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenThen (AlexPn _ a b)) = show a ++ " : " ++ show b
token_posn ( TokenLambda (AlexPn _ a b)) = show a ++ " : " ++ show b

}

