module AST where
import Token

data AST = AST ASTNode Children
  deriving(Show)

data ASTNode = 
  Terminal {
    original :: Token,
    tokentype :: SymbolType 
     }deriving(Show)

data SymbolType = I | S | N
  deriving(Show)

type Children = [AST]
type TokenAST = (TokenList, AST)


newNode :: Token -> ASTNode
newNode token 
  |tt == IntOp || tt == PlusOp || tt == MinusOp = Terminal token I 
  |tt == CharOp = Terminal token S 
  |otherwise = Terminal token N 
  where tt = kind token


addParentNode :: AST -> ASTNode -> AST
addParentNode child parent = addChildTree (AST parent []) child 

addParentTree :: AST -> AST -> AST
addParentTree child parent = addChildTree parent child 

addChildTree :: AST -> AST -> AST
addChildTree (AST root children) child = AST root $ child : children

addChildNode :: AST -> ASTNode -> AST
addChildNode (AST root children) child = AST root $ (AST child []) : children
