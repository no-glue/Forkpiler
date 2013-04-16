module SemanticAnalysis where
import Token
import qualified Data.Map as Map
import AST
import SymbolTable 

--analyze :: AST -> AST
--analyze (AST parent (child:children))
  -- |tt == OpenBrace = block child:children 
  -- |tt == PlusOp || tt == MinusOp = math
 -- where tt = kind $ original parent

--block :: [AST] -> AST
--block  (AST parent 

--math :: [AST] -> AST
--math [] = []
--math (child1:child2) 

  --where
   -- ast1 = AST 
 
fullyTypeTree :: AST -> ScopeMap -> AST
fullyTypeTree tree = typeingLine (tree, m, (0))

typeingLine :: (AST, ScopeMap, Int) -> AST
typeingLine (AST parent kids, m, scope)
  |tt == ID = let fosterParent = typeID (parent, m, scope)
              in addChildNode fosterParent kids
  |tt == OpenBrace = let fosterKids = typeingLineBrace kids (m, scope+1)
                     in addChildTree parent fosterKids  
  |otherwise = let fosterParent = typeingLine (kids, m, scope)
               in addChildNode fosterParent kids
  where
    (child:children) = kids
    tt = kind $ original parent 

typeingLineBrace :: Children -> (ScopeMap, Int) -> Children
typeingLineBrace [] _ = []
brace (kid:kids) (m, scope) =
  let fosterBrother = typeingLine (kid, m, scope)
  in fosterBrother : brace kids

typeID :: (ASTNode, ScopeMap, Int) -> ASTNode
typeID (node, m, scope) = ASTNode ori I 
  where ori = original node

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
