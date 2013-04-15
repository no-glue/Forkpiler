module SemanticAnalysis where
import Token
import AST
import ScopeTree

--analyze :: AST -> AST
analyze (AST parent (child:children))
  |tt == OpenBrace = block child:children 
  |tt == PlusOp || tt == MinusOp = math
  where tt = kind $ original parent

block :: [AST] -> AST
block  (AST parent 
