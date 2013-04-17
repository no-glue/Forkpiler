{-# LANGUAGE BangPatterns #-}
module SemanticAnalysis where
import Token
import qualified Data.Map as Map
import AST
import SymbolTable 
import Debug.Trace 

typeCheck :: AST -> ScopeMap -> Int -> Symbol
typeCheck (AST ast children) m scope
  |tt == CharacterList = dummySymbol S 
  |tt == Digit = dummySymbol I
  |tt == EqualsOp = dummySymbol $ assignType children m scope
  |tt == PrintOp = dummySymbol $ printType children m scope
  |tt == PlusOp || tt == MinusOp = dummySymbol $ mathType children m scope
  |tt == OpenBrace = dummySymbol $ head $! typeCheckChildren children m (scope+1)
  |tt == IntOp = dummySymbol I 
  |tt == CharOp = dummySymbol S
  |tt == ID = getSymbol 
  where 
    tt = kind $ original ast
    key = contents $ original ast
    line = location $ original ast
    dummySymbol t = Symbol "dummy" (-1) t "" False
    getSymbol
     |symbol == Errer = error("Undecleraed ID: " ++ key ++ " on Line: " ++ (show line))
     |otherwise = symbol
     where symbol = findInScope m key scope

typeCheckChildren :: [AST] -> ScopeMap -> Int -> [SymbolType]
typeCheckChildren [] m scope = [] 
typeCheckChildren (child: childs) m scope = 
    childScope : kidsScope
  where
    !childScope = sType $ typeCheck child m scope
    !kidsScope = typeCheckChildren childs m scope

assignType :: [AST] -> ScopeMap -> Int -> SymbolType
assignType (child1:child2:[]) m scope
  |lt /= rt = error("Type mismatch in assignment of: "  ++ (name left)
    ++" Found: " ++ (expandType rt) ++ " Expected: " ++ (expandType lt)
    ++ " on line: " ++ (show line)) 
  |otherwise = lt
  where 
    !left = typeCheck child1 m scope 
    !right = typeCheck child2 m scope
    lt = sType left
    rt = sType right
    AST node _  = child1
    line = location $ original node

mathType :: [AST] -> ScopeMap -> Int -> SymbolType
mathType (child1:child2:[]) m scope
  |lt == S = error("Error: Found String on left of plus. Line: " ++ (show line)
        ++ "\n Note: Plus is not for String concatenation") 
  |rt == S = error("Error: Found String on right of plus. Line: " ++ (show line)
      ++ " \nNote: Plus is not for String concatenation")
  |otherwise = I
  where
    !left = typeCheck child1 m scope
    !right = typeCheck child2 m scope 
    lt = sType left
    rt = sType right
    AST node _  = child1
    line = location $ original node

printType :: [AST] -> ScopeMap -> Int -> SymbolType
printType (child1:[]) m scope 
  |tt == PlusOp = mathType children m scope
  |tt == CharacterList = S
  |tt == Digit = I
  |otherwise = sType $ typeCheck child1 m scope
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
  let !nm = symbolLine (kid, m,(pscope,scope)) 
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
