module SymbolTable where
import Data.Map
import Token
import AST


data Symbol = Symbol {
  name :: String,
  address :: Int,
  sType :: SymbolType,
  value :: Int,
  scope :: Int,
  used :: Bool
}deriving(Show)

type SymbolTable = Map String [Symbol]

insertSymbol :: SymbolTable -> Symbol -> SymbolTable 
insertSymbol table symbol 
  |member key table = insert key (symbol:(table ! key)) table 
  |otherwise = insert key [symbol] table
  where key = name symbol
