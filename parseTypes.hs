module ParseTypes(Statement, Exper, End) where

data Statement = Statement | Exper End 

data Exper = Int | Char

data End = Monday deriving(Enum)
