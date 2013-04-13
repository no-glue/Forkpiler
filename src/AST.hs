module AST where
import Token

data AST = AST ASTNode Children
  deriving(Show)

data ASTNode = 
  Terminal {original :: Token} 
  |Program
  |Statement
  |StatementList
  |Expr
  |IntExpr
  |StringExpr
  |CharList
  |VarDecl
  deriving(Show)

type Children = [AST]
type TokenAST = (TokenList, AST)

addParentNode :: AST -> ASTNode -> AST
addParentNode child parent = addChildTree (AST parent []) child 

addParentTree :: AST -> AST -> AST
addParentTree child parent = addChildTree parent child 

addChildTree :: AST -> AST -> AST
addChildTree (AST root children) child = AST root $ child : children

addChildNode :: AST -> ASTNode -> AST
addChildNode (AST root children) child = AST root $ (AST child []) : children
