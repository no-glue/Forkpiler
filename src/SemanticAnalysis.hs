module SemanticAnalysis where
import Token
import qualified Data.Map as Map
import AST
import SymbolTable 
import Debug.Trace 

typeCheck :: AST -> ScopeMap -> Int -> SymbolType
typeCheck (AST ast children) m scope
  |tt == CharacterList = trace("Found String") S
  |tt == Digit = I
  |tt == EqualsOp = assignType children m scope
  |tt == PrintOp = printType children m scope
  |tt == PlusOp || tt == MinusOp = mathType children m scope
  |tt == OpenBrace = error("found brace") head $ typeCheckChildren children m (scope+1)
  |tt == IntOp = trace("Found decleration") I
  |tt == CharOp = S
  |otherwise = error("found" ++ show ast)
  where tt = kind $ original ast

typeCheckChildren :: [AST] -> ScopeMap -> Int -> [SymbolType]
typeCheckChildren (child: childs) m scope = 
  typeCheck child m scope : typeCheckChildren childs m scope

assignType :: [AST] -> ScopeMap -> Int -> SymbolType
assignType (child1:child2:[]) m scope
  |left /= right = error("Type mismatch in assignment. "
    ++"Found something" ++ (show $ right) ++ "something else on line number number") 
  |otherwise = left
  where 
    left = typeCheck child1 m scope 
    right = typeCheck child1 m scope

mathType :: [AST] -> ScopeMap -> Int -> SymbolType
mathType (child1:child2:[]) m scope
  |left == S = error("Error: Found String on left of plus. Note: " 
        ++"Plus is not for String concatenation")
  |right == S = error("Error: Found String on right of plus. Note: "
       ++ "Plus is not for String concatenation")
  |otherwise = trace("Math typechecks") I
  where
    left = typeCheck child1 m scope
    right = typeCheck child2 m scope 

printType :: [AST] -> ScopeMap -> Int -> SymbolType
printType (child1:[]) m scope 
  |tt == PlusOp = mathType children m scope
  |tt == CharacterList = S
  |tt == Digit = I
  |otherwise = typeCheck child1 m scope
  where 
    tt = kind $ original parent
    AST parent children = child1 

buildSymbolTable :: AST -> ScopeMap
buildSymbolTable tree = symbolLine (tree , Map.empty, (0,0))

symbolLine :: (AST, ScopeMap, Scope) -> ScopeMap
symbolLine (AST parent [], m, (pscope, scope)) = m
symbolLine (AST parent childs, m, (pscope, scope))
  |tt == IntOp = intScope (child, m, (pscope, scope))
  |tt == CharOp = charScope (child, m, (pscope, scope))
  |tt == OpenBrace = brace childs (m,(scope,scope+1))
  |otherwise = symbolLine (child, m, (pscope, scope))
  where 
    (child:children) = childs
    tt = kind $ original parent

brace :: Children -> (ScopeMap, Scope) -> ScopeMap
brace [] (m,_) = m 
brace (kid:kids) (m, (pscope,scope)) =
  let nm = symbolLine (kid, m,(pscope,scope)) 
  in brace kids (nm, (pscope,scope))

intScope :: (AST, ScopeMap, Scope) -> ScopeMap
intScope ((AST parent([])), m, scope) = updatedMap
    where 
      updatedMap = insertInScope m symbol scope
      symbol = tokenAndTypeToSymbol t I
      t = original parent

charScope :: (AST, ScopeMap, Scope) -> ScopeMap
charScope ((AST parent([])), m, scope) = updatedMap
    where 
      updatedMap = insertInScope m symbol scope
      symbol = tokenAndTypeToSymbol t S
      t = original parent
