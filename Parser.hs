module Parser where

import Token
import Debug.Trace
import SymbolTable
--Ok so this took way to long to realise how to write properly
--I blame it on the class being taught imparativly :p
--Anyways the big break through was that when parsing something like
--(stuff) in a totally recursive way you have to call the functions
--in semingly reverse order. I should have realised that at first
--but for some reason it didn't seem right. 
--The exit conditiono for almost all of the recursive calls is to 
--just return rest. 

parse :: [Token] -> ([Token], SymbolTable) 
parse tokens = empty $ program tokens

program :: [Token] -> ([Token], SymbolTable)
program tokens = statement (tokens, [])

statement :: ([Token], SymbolTable) -> ([Token], SymbolTable)
statement ((token:rest),table)
  |kind token == PrintOp =
    consumeToken ParenClose (exper $ consumeToken ParenOpen $ 
    trace("parsing open paren") (rest, table))
  |kind token == ID = 
    exper . consumeToken EqualsOp $ trace("parsing ID statement") (rest,table)
  |kind token == OpenBrace = 
    statementList $ trace("parsing open brace") (rest,table)
  |kind token == IntOp = varDecl $ trace("parsing IntOp") (rest,table)
  |kind token == CharOp = varDecl $ trace("parsing CharOp") (rest,table)
  |otherwise = error("Expecting more in statement found " ++ (show token)) 
  
exper :: ([Token], SymbolTable) -> ([Token], SymbolTable)
exper ((token:rest),table)
  |kind token == Digit = 
    intExper $ trace("parsing Digit " ++ (show token)) (rest,table)
  |kind token == CharacterList = trace("parsed character list in exper") (rest,table)
  |kind token == ID = trace("parsed ID in exper") (rest,table)
  |otherwise = error("Error in exper found " ++ (show token) ++ " most likely a lone operator")

varDecl :: ([Token], SymbolTable) -> ([Token], SymbolTable)
varDecl ((token:rest),table)
  |kind token == ID = trace("parsing variable decleration") (rest,table)
  |otherwise = error("varDecl error with token" ++(show token))

statementList :: ([Token], SymbolTable) -> ([Token], SymbolTable)
statementList ([],table) = ([],table)
statementList ((token:rest),table)
  |kind token == CloseBrace = trace("parsing end of statement list") $ (rest,table)
  |length rest == 1 = consumeToken CloseBrace (rest,table)
  |otherwise  = statementList $!  
    statement $ trace("parsing statementList at " ++ (show token)) $ ((token:rest),table) 

intExper :: ([Token], SymbolTable) -> ([Token], SymbolTable)
intExper ((token:rest),table)
  |kind token == PlusOp = exper $ trace("parsed PlusOp") (rest,table)
  |kind token == MinusOp = exper $ trace("parsed MinusOp") (rest,table)
  |otherwise = trace("Done intExper") ((token:rest),table)

consumeToken :: TokenType -> ([Token],SymbolTable) -> ([Token],SymbolTable)
consumeToken type' ([],_) = error("Looking for " ++ (show type') ++ " found nothing")
consumeToken type' ((token:rest),table)
  |kind token == type' = trace("consuming " ++(show  token)) $  (rest,table)
  |otherwise = error("expected: " ++ (show type') ++ " got " ++ (show token))

empty :: ([Token], SymbolTable) -> ([Token], SymbolTable)
empty ([],table) = ([],table)
empty (x,_) = error("Code outside of {}. Maybe missing {}? Tokens look like " ++
  (show x)) 
