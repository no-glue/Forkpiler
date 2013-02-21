module Token (Token(..)) where

data Token = Token {
  contents :: String,
  location :: (Int,Int),
  kind :: String
} | Warning{
    kind :: String}
  | Error{
    kind :: String}deriving(Show,Eq)
