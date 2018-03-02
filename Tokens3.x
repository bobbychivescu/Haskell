{ 
module Tokens3 where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 

tokens :-
$white+       ; 
  "--".*        ; 
  f           { \s -> TokenForward } 
  $digit+     { \s -> TokenInt (read s) } 
  c           { \s -> TokenCheck }
  rt          { \s -> TokenRotate }
  l           { \s -> TokenLeft}
  r           { \s -> TokenRight }
  if          { \s -> TokenIf }
  then        { \s -> TokenThen }
  else        { \s -> TokenElse }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  end         { \s -> TokenEnd }
  

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenForward  | 
  TokenInt Int  |
  TokenCheck    | 
  TokenRotate   |
  TokenLeft     |
  TokenRight    |
  TokenIf       |
  TokenThen     |
  TokenLParen   |
  TokenRParen   |
  TokenElse     |
  TokenEnd
  deriving (Eq,Show) 


}

