module AST where
import Token

data AST = AST ASTNode Children

data ASTNode = 
  Terminal {
    original :: Token,
    tokentype :: SymbolType 
   }

data SymbolType = I | S | B | N{scope :: Int}
  deriving(Show,Eq)

instance Show ASTNode where 
  show node  
    |(kind $ original node) == OpenBrace = let token = original node
      in (show $ kind token) ++ " " ++ (contents token) ++ " " ++ (show $ tokentype node)
    |otherwise = let token = original node
      in (show $ kind token) ++ " " ++ (contents token)
    
instance Show AST where
  show ast = drawTree ast

type Children = [AST]
type TokenAST = (TokenList, AST)


newNode :: Token -> ASTNode
newNode token 
  |tt == Digit || tt == IntOp || tt == PlusOp || tt == MinusOp = Terminal token I 
  |tt == CharOp || tt == CharacterList = Terminal token S 
  |tt == TrueOp || tt == FalseOp = Terminal token B
  |tt == OpenBrace = Terminal token (N 0)
  |otherwise = Terminal token (N 0)
  where tt = kind token


addParentNode :: AST -> ASTNode -> AST
addParentNode child parent = addChildTree (AST parent []) child 

addParentTree :: AST -> AST -> AST
addParentTree child parent = addChildTree parent child 

addChildTree :: AST -> AST -> AST
addChildTree (AST root children) child = AST root $ children ++ [child] 

addChildNode :: AST -> ASTNode -> AST
addChildNode (AST root children) child = AST root $ children ++ [(AST child [])] 

addChildren :: AST -> Children -> AST
addChildren (AST root children) kids = AST root $ children ++ kids

drawTree = unlines . draw

draw (AST node children) = show node : drawSub children
  where 
    drawSub [] = []
    drawSub [t] = "|" : shift "\\-" "  " (draw t)
    drawSub (t:ts) = "|" : shift "\\-" "  " (draw t) ++ drawSub ts
    shift first other = zipWith (++) (first : repeat other)   
