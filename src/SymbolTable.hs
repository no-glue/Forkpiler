module SymbolTable where
import qualified Data.Map as Map
import Token
import AST
import Debug.Trace

data Symbol = Symbol{
  name    :: String,
  address :: Int,
  sType   :: SymbolType,
  value   :: String,
  used    :: Bool
}|Errer deriving(Show, Eq)

type SymbolTable = Map.Map String (Symbol,Int)
type ScopeMap = Map.Map Int SymbolTable
type Scope = (Int, Int)

prettyPrint :: ScopeMap -> String 
prettyPrint m = trace "Symbol Table\n" 
  Map.foldlWithKey prettyify "" m
  where 
    prettyify sum k a = Map.foldlWithKey subPretty 
      (trace("Scope: " ++ (show k) ++ "\n") sum) a
    subPretty sum2 k2 a2
      |n == "parent" = sum2
      |otherwise = "Symbol: " ++ n ++ " With Type: " 
        ++ t ++ " and Value: " ++ v ++ "\n" ++ sum2
      where
        (symbol, scope) = a2
        n = name symbol
        t = show $ sType symbol
        v = value symbol

expandType :: SymbolType -> String
expandType t
  |t == S = "string"
  |t == I = "int"
  |otherwise = "no type"

tokenAndTypeToSymbol :: Token -> SymbolType -> Symbol
tokenAndTypeToSymbol token ty = 
  Symbol{name = c, address = l, sType = ty, value = "", used = False}
  where
    c = contents token
    l = location token

insertInScope :: ScopeMap -> Symbol -> Scope -> ScopeMap
insertInScope m symbol (pscope,scope)
  |Map.member scope m = Map.adjust (insertSymbol) scope m
  |otherwise = Map.insert scope (Map.fromList[(key,(symbol,pscope))]) m
  where
    key = name symbol
    insertSymbol table
      |Map.member key table && key /= "parent" = 
        error("Redecleration of Symbol " ++ key ++ " on line " ++ (show $ location)) 
      |otherwise = Map.insert key (symbol,pscope) table
    location = address symbol

findInScope :: ScopeMap -> String -> Int -> Symbol
findInScope m key scope 
  |Map.member scope m = 
    let table = m Map.! scope
    in 
    if Map.member key table
       then fst (table Map.! key)
       else if scope /= 0
         then findInScope m key (parent table)
         else cantFind
  |otherwise = cantFind
   where 
    parent = snd . head . Map.elems
    cantFind =  trace ("couldn't find " ++ (show scope)) Errer 

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
  Map.adjust (insertSymbol) scope m
  where 
    key = name symbol 
    insertSymbol table = Map.insert key (symbol,pscope) table

use :: ScopeMap -> String -> Scope -> ScopeMap
use m key (pscope,scope) = SymbolTable.update m symbol (pscope,scope)
  where
    s = findInScope m key scope
    symbol = Symbol (name s) (address s) (sType s) (value s) True

warnUsedButUnintilized :: ScopeMap -> [Symbol]
warnUsedButUnintilized m = filter (test) (compress m)
  where
    test s = 
      let Symbol _ _ _ val use = s
      in if (val == "" && use) then True else False 

warnDecleredButUnUsed :: ScopeMap -> [Symbol]
warnDecleredButUnUsed m = filter (test) (compress m)
  where
    test s =
      let Symbol _ _ _ _ use = s
      in not use

compress :: ScopeMap -> [Symbol]
compress m = symbols
  where 
    listyList = Prelude.map Map.elems (Map.elems m)
    listy = concat listyList
    symbols = map fst listy

emptySymbolTable :: ScopeMap
emptySymbolTable = Map.empty--Map.fromList [(0, (Map.empty))]
