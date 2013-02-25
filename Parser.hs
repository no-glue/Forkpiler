module Parser where

import Token
--Ok so this took way to long to realise how to write properly
--I blame it on the class being taught imparativly :p
--Anyways the big break through was that when parsing something like
--(stuff) in a totally recursive way you have to call the functions
--in semingly reverse order. I should have realised that at first
--but for some reason it didn't seem right. 
--The exit conditiono for almost all of the recursive calls is to 
--just return rest. 

parse tokens = statement tokens

statement (token:rest)
  |kind token == PrintOp =
    consumeToken ParenClose (exper . consumeToken ParenOpen $ rest)
  |kind token == ID = 
    exper . consumeToken EqualsOp $ rest
  |kind token == OpenBrace = 
    consumeToken CloseBrace (statementList  rest)
  |kind token == IntOp = varDecl rest
  |kind token == CharOp = varDecl rest
  --to match epislon of statementList nothing is consumed
  |otherwise = token:rest
  
exper (token:rest)
  |kind token == Digit = 
    intExper rest
  |kind token == CharacterList = rest
  |kind token == ID = rest

varDecl (token:rest)
  |kind token == IntOp = 
    consumeToken ID rest
  |kind token == CharOp = 
    consumeToken ID rest

statementList tokens = statement tokens

intExper (token:rest) = exper (op rest)

op (token:rest)
  |kind token == PlusOp = rest
  |kind token == MinusOp = rest
  --error condition

consumeToken :: TokenType -> [Token] -> [Token]
consumeToken type' (token:rest)
  |kind token == type' = rest
  --error condition 
