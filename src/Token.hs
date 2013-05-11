module Token where

data TokenType = ParenOpen
  |ParenClose
  |EqualsOp
  |PlusOp
  |MinusOp
  |OpenBrace
  |CloseBrace
  |Digit
  |CharacterList
  |IntOp
  |CharOp
  |ID
  |EOF
  |PrintOp
  |Equality
  |While
  |If
  |Boolean
  |Error
  |Warning
  deriving(Eq, Ord, Enum, Show)


data Token = Token {
  contents :: String,
  location :: Int,
  kind :: TokenType 
}

type TokenList = [Token]

instance Show Token where
  show token = (show $ kind token) ++ " token at line:" 
    ++ (show $ location token) 
    ++ " with contents " ++ (contents token)

instance Eq Token where 
  x == y = kind x == kind y
  x /= y = kind x /= kind y

findToken :: TokenList -> TokenType -> Int
findToken [] _ = -1
findToken xs goal = loop 0 xs
  where 
    loop _ [] = -1 
    loop n (y:ys) 
      |kind y == goal = n
      |otherwise = loop (n+1) ys


printLocation :: Token -> String
printLocation token = show $ location token

printKind :: Token -> String
printKind token = show $ kind token

testList :: TokenList
testList = [(Token "1" (-1) EqualsOp), (Token "2" 2 EOF)]
