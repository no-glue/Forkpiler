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
  |Error
  |Warning
  deriving(Eq, Ord, Enum, Show)


data Token = Token {
  contents :: String,
  location :: Int,
  kind :: TokenType 
}

instance Show Token where
  show token = (kind token) ++ " token at line:" 
    ++ (show $ location token) 
    ++ " with contents " ++ (contents token)
