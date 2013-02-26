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

--entry point and also catches the error of there being a missing } at the end
parse :: [Token] -> ([Token], SymbolTable) 
parse tokens = empty $ program tokens

--the first rewrite rule. does nothing but be called by parse
program :: [Token] -> ([Token], SymbolTable)
program tokens = statement (tokens, [])

--The big one. Catches most of the beginnings of complex statements
statement :: ([Token], SymbolTable) -> ([Token], SymbolTable)
statement ((token:next:rest),table)
  |kind token == PrintOp =
    consumeToken ParenClose (exper $ consumeToken ParenOpen $ 
    trace("parsing open paren") (next:rest, table))
  |kind token == ID = 
    exper . consumeToken EqualsOp $ trace("parsing ID statement") (next:rest,table)
  |kind token == OpenBrace = 
    statementList $ trace("parsing open brace") (next:rest,table)
  |kind token == IntOp = varDecl $ trace("parsing IntOp") (next:rest, insertSymbol table ([token,next]))
  |kind token == CharOp = varDecl $ trace("parsing CharOp") (next:rest,table)
  |otherwise = error("Expecting more in statement found " ++ (show token)) 
  
--parses the expression rule and inserts symbols into the symbol table
exper :: ([Token], SymbolTable) -> ([Token], SymbolTable)
exper ((token:rest),table)
  |kind token == Digit = 
    intExper $ trace("parsing Digit " ++ (show token)) (rest,table)
  |kind token == CharacterList = trace("parsed character list in exper") (rest,table)
  |kind token == ID = trace("parsed ID in exper") (rest,table)
  |otherwise = error("Error in exper found " ++ (show token) ++ " most likely a lone operator")

--parses the existance of an ID after a type decleration
varDecl :: ([Token], SymbolTable) -> ([Token], SymbolTable)
varDecl ((token:rest),table)
  |kind token == ID = trace("parsing variable decleration") (rest,table)
  |otherwise = error("varDecl error with token" ++(show token))

--the big recursive guy. $! is 100% needed. if missing Haskell will be super lazy
--and recurse FOREVER by not consuming anything and instead just running in cicrles
statementList :: ([Token], SymbolTable) -> ([Token], SymbolTable)
statementList ([],table) = ([],table)
statementList ((token:rest),table)
  |kind token == CloseBrace = trace("parsing end of statement list") $ (rest,table)
  --not certain why it has to be 1 here and not 0 but it works...
  |length rest == 1 = consumeToken CloseBrace (rest,table)
  |otherwise  = statementList $!  
    statement $ trace("parsing statementList at " ++ (show token)) $ ((token:rest),table) 

--parses recursive int expressions. does not calculate any values for symbols that's in
--sementic analysis
intExper :: ([Token], SymbolTable) -> ([Token], SymbolTable)
intExper ((token:rest),table)
  |kind token == PlusOp = exper $ trace("parsed PlusOp") (rest,table)
  |kind token == MinusOp = exper $ trace("parsed MinusOp") (rest,table)
  |otherwise = trace("Done intExper") ((token:rest),table)

--nom nom nom
consumeToken :: TokenType -> ([Token],SymbolTable) -> ([Token],SymbolTable)
consumeToken type' ([],_) = error("Looking for " ++ (show type') ++ " found nothing")
consumeToken type' ((token:rest),table)
  |kind token == type' = trace("consuming " ++(show  token)) $  (rest,table)
  |otherwise = error("expected: " ++ (show type') ++ " got " ++ (show token))

empty :: ([Token], SymbolTable) -> ([Token], SymbolTable)
empty ([],table) = ([],table)
empty (x,_) = error("Code outside of {}. Maybe missing {}? Tokens look like " ++
  (show x)) 
