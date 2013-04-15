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

math :: [AST] -> AST
math [] = []
math (child1:child2:others) = 
math ((AST node1 type1) : (AST node2 type2)) 
  |type1 /= I = error("Type mismatch on line")
  |type2 /= I = error("Type mismatch on line")
  |otherwise = []
