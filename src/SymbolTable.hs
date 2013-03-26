module SymbolTable where
import Data.Char
import Token

data SymbolType = SInt | SChar
  deriving(Eq, Ord, Bounded, Enum, Show)

data Symbol = Symbol {
  name :: String,
  address :: Int,
  sType :: SymbolType,
  value :: Int,
  used :: Bool
}deriving(Show)

intSymbol :: Symbol
intSymbol = Symbol { sType = SInt, value = 0, used = False }

charSymbol :: Symbol
charSymbol = Symbol { sType = SChar, value = ord 'a', used = False }

type SymbolTable = [Symbol] 

insertSymbol :: SymbolTable -> [Token] -> SymbolTable
insertSymbol table (token:tokens)
  |kind token == IntOp = intSymbol {name = (contents $ head tokens), address = (location token)} : table
  |kind token == CharOp = charSymbol {name = (contents $ head tokens), address = (location token)} : table
  |otherwise = error("Error inserting " ++ (show token) ++ " into symbol table")

-- updateValue :: SymbolTable -> [Token] -> SymbolTable
-- updateValue table (token:tokens)
  -- |otherwise = error("not written yet don't use me yo")
