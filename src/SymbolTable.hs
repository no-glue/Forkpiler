module SymbolTable where
import Data.Map
import Token
import AST

data Symbol = Symbol {
  name :: String,
  address :: Int,
  sType :: SymbolType,
  value :: Int,
  used :: Bool
}|Errer
  deriving(Show, Eq)

type SymbolTable = Map String (Symbol,Int)
type ScopeMap = Map Int SymbolTable

insertInScope :: ScopeMap -> Symbol -> Int -> Int -> ScopeMap
insertInScope m symbol pscope scope
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
  |otherwise = Symbol "error" 1 I 1 False  
  where parent = snd . head . elems

setValue :: ScopeMap -> Token -> Int -> Int -> ScopeMap
setValue m t scope v 
  |symbol == Errer = error("uh oh")
  where 
    symbol = findInScope m key scope 
    key = contents t 
