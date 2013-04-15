module AST where
import Token

data AST = AST ASTNode Children

data ASTNode = 
  Terminal {
    original :: Token,
    tokentype :: SymbolType 
   }

data SymbolType = I | S | N
  deriving(Show)

instance Show ASTNode where 
  show node = let token = original node
    in (show $ kind token) ++ " " ++ (contents token) 

instance Show AST where
  show ast = drawTree ast

type Children = [AST]
type TokenAST = (TokenList, AST)


newNode :: Token -> ASTNode
newNode token 
  |tt == Digit || tt == IntOp || tt == PlusOp || tt == MinusOp = Terminal token I 
  |tt == CharOp = Terminal token S 
  |otherwise = Terminal token N 
  where tt = kind token


addParentNode :: AST -> ASTNode -> AST
addParentNode child parent = addChildTree (AST parent []) child 

addParentTree :: AST -> AST -> AST
addParentTree child parent = addChildTree parent child 

addChildTree :: AST -> AST -> AST
addChildTree (AST root children) child = AST root $ children ++ [child] 

addChildNode :: AST -> ASTNode -> AST
addChildNode (AST root children) child = AST root $ children ++ [(AST child [])] 

drawTree = unlines . draw

draw (AST node children) = show node : drawSub children
  where 
    drawSub [] = []
    drawSub [t] = "|" : shift "\\-" "  " (draw t)
    drawSub (t:ts) = "|" : shift "\\-" "  " (draw t) ++ drawSub ts
    shift first other = zipWith (++) (first : repeat other)   
