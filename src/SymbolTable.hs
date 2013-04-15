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

type SymbolTable = Map String [(Symbol,Int)]
type ScopeMap = Map Int SymbolTable

insertSymbol :: SymbolTable -> Symbol -> SymbolTable 
insertSymbol table symbol 
  |member key table = error("Redecleration of Symbol " ++ key ++ " on line " ++ location) 
  |otherwise = insert key [symbol] table
  where key = name symbol
    location = address symbol

findInScope :: ScopeMap -> Symbol -> Int 
findInScope table symbol scope 
   |member key table = pluck (table ! key) scope
   where pluck = 
    key = name symbol
