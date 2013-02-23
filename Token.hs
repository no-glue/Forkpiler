module Token (Token(..)) where

data Token = Token {
  contents :: String,
  location :: Int,
  kind :: String
}

instance Show Token where
  show token = (kind token) ++ " token at line:" 
    ++ (show $ location token) 
    ++ " with contents " ++ (contents token)
