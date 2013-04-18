module SymbolTable where
import Data.Map
import Token
import AST

data Symbol = Symbol{
  name :: String,
  address :: Int,
  sType :: SymbolType,
  value :: String,
  used :: Bool
}|Errer deriving(Show, Eq)

type SymbolTable = Map String (Symbol,Int)
type ScopeMap = Map Int SymbolTable
type Scope = (Int, Int)

expandType :: SymbolType -> String
expandType t 
  |t == S = "string"
  |t == I = "int"
  |otherwise = "no type"

tokenAndTypeToSymbol :: Token -> SymbolType -> Symbol
tokenAndTypeToSymbol token ty = 
  Symbol{name = c, address = l, sType = ty, value = c, used = False}
  where 
    c = contents token
    l = location token

insertInScope :: ScopeMap -> Symbol -> Scope -> ScopeMap
insertInScope m symbol (pscope,scope)
  |member scope m = adjust (insertSymbol) scope m
  |otherwise = insert scope (fromList[(key,(symbol,pscope))]) m
  where
    key = name symbol 
    insertSymbol table
      |member key table = 
        error("Redecleration of Symbol " ++ key ++ " on line " ++ (show $ location)) 
      |otherwise = insert key (symbol,pscope) table
    location = address symbol

findInScope :: ScopeMap -> String -> Int -> Symbol
findInScope m key scope 
  |member scope m = 
    let table = m ! scope
    in if member key table
       then fst (table ! key)
       else findInScope m key (parent table)
  |otherwise = Errer 
  where parent = snd . head . elems

updateValue :: ScopeMap -> Token -> Scope -> String -> ScopeMap
updateValue m t (pscope,scope) v 
  |symbol == Errer = error(undecleraed)
  |otherwise =
    let newSymbol = Symbol (name symbol) (address symbol) (sType symbol) v True 
    in SymbolTable.update m newSymbol (pscope,scope)
  where 
    symbol = findInScope m key scope 
    key = contents t 
    undecleraed = "Attempt to assign value to undecleraed identifier " ++
      key ++ " on line " ++ (show $ location t) 

update :: ScopeMap -> Symbol -> (Int,Int) -> ScopeMap
update m symbol (pscope,scope) =
  adjust (insertSymbol) scope m
  where 
    key = name symbol 
    insertSymbol table = insert key (symbol,pscope) table

use :: ScopeMap -> String -> Scope -> ScopeMap
use m key (pscope,scope) = SymbolTable.update m symbol (pscope,scope)
  where
    s = findInScope m key scope
    symbol = Symbol (name s) (address s) (sType s) (value s) True
