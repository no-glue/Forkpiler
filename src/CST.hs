module CST where
import Token

data CST = CST CSTNode Children
  deriving(Show)

data CSTNode = 
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

type Children = [CST]
type TokenCST = (TokenList, CST)

addParentNode :: CST -> CSTNode -> CST
addParentNode child parent = addChildTree (CST parent []) child 

addParentTree :: CST -> CST -> CST
addParentTree child parent = addChildTree parent child 

addChildTree :: CST -> CST -> CST
addChildTree (CST root children) child = CST root $ child : children

addChildNode :: CST -> CSTNode -> CST
addChildNode (CST root children) child = CST root $ (CST child []) : children
