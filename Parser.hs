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
    consumeToken ParenClose (exper $ consumeToken ParenOpen $ 
    trace("parsing open paren")rest)
  |kind token == ID = 
    exper . consumeToken EqualsOp $ trace("parsing ID statement") rest
  |kind token == OpenBrace = 
    statementList $ trace("parsing open brace") rest
  |kind token == IntOp = varDecl $ trace("parsing IntOp") rest
  |kind token == CharOp = varDecl $ trace("parsing CharOp") rest
  |otherwise = error("Expecting more in statement found " ++ (show token)) 
  
exper :: [Token] -> [Token]
exper (token:rest)
  |kind token == Digit = 
    intExper $ trace("parsing Digit " ++ (show token)) rest
  |kind token == CharacterList = trace("parsed character list in exper") rest
  |kind token == ID = trace("parsed ID in exper") rest

varDecl :: [Token] -> [Token]
varDecl (token:rest)
  |kind token == ID = trace("parsing variable decleration") rest
  |otherwise = error("varDecl error with token" ++(show token))

statementList :: [Token] -> [Token]
statementList [] = []
statementList (token:rest) 
--  |kind token == CloseBrace = trace("parsing end of statement list") $ rest
    = consumeToken CloseBrace $! statementList $!  
    statement $ trace("parsing statementList at " ++ (show token)) $ token:rest 

intExper :: [Token] -> [Token]
intExper (token:rest)
  |kind token == PlusOp = exper $ trace("parsed PlusOp") rest
  |kind token == MinusOp = exper $ trace("parsed MinusOp") rest
  |otherwise = trace("Done intExper") token:rest

op :: [Token] -> [Token]
op (token:rest)
  |kind token == PlusOp = rest
  |kind token == MinusOp = rest
  --error condition

consumeToken :: TokenType -> [Token] -> [Token]
consumeToken type' [] = error("Looking for " ++ (show type') ++ " found nothing")
consumeToken type' (token:rest)
  |kind token == type' = trace("consuming " ++(show  token)) $  rest
  |otherwise = error("expected: " ++ (show type') ++ " got " ++ (show token))
