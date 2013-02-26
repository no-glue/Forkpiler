module Parser where

import Token
import Debug.Trace
--Ok so this took way to long to realise how to write properly
--I blame it on the class being taught imparativly :p
--Anyways the big break through was that when parsing something like
--(stuff) in a totally recursive way you have to call the functions
--in semingly reverse order. I should have realised that at first
--but for some reason it didn't seem right. 
--The exit conditiono for almost all of the recursive calls is to 
--just return rest. 

parse :: [Token] -> [Token]
parse tokens = statement tokens

statement :: [Token] -> [Token]
statement (token:rest)
  |kind token == PrintOp =
    consumeToken ParenClose (exper . consumeToken ParenOpen $ 
    trace("parsing open paren")rest)
  |kind token == ID = 
    exper . consumeToken EqualsOp $ trace("parsing ID statement") rest
  |kind token == OpenBrace = 
    statementList $ trace("parsing open brace") rest
  |kind token == IntOp = varDecl $ trace("parsing IntOp") rest
  |kind token == CharOp = varDecl $ trace("parsing CharOp") rest
  --to match epislon of statementList nothing is consumed
  -- |otherwise = statement $ trace("parsing another statement") token:rest
  
exper :: [Token] -> [Token]
exper (token:rest)
  |kind token == Digit = 
    intExper rest
  |kind token == CharacterList = rest
  |kind token == ID = rest

varDecl :: [Token] -> [Token]
varDecl (token:rest)
  |kind token == ID = trace("parsing ID") rest
  |otherwise = error("varDecl error with token" ++(show token))

statementList :: [Token] -> [Token]
statementList (token:rest) 
  |kind token == CloseBrace = trace("parsing end of statement list") $ rest
  |otherwise = statementList $ trace("parsing second bit of statement list") `seq` statement$ trace("parsing statementList at token" ++ (show token)) $ token:rest 

intExper :: [Token] -> [Token]
intExper (token:rest) = exper (op rest)

op :: [Token] -> [Token]
op (token:rest)
  |kind token == PlusOp = rest
  |kind token == MinusOp = rest
  --error condition

consumeToken :: TokenType -> [Token] -> [Token]
consumeToken type' (token:rest)
  |kind token == type' = trace("consuming " ++(show  token)) $  rest
  |otherwise = error("expected: " ++ (show type') ++ " got " ++ (show token))
