module Token (Token(..)) where

data Token = Token {
  contents :: String,
  location :: (Int,Int),
  kind :: String
}deriving (Show, Eq)
