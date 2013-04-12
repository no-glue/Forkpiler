module MParser where

import Token
import CST
import Debug.Trace
import ParserHelpers

--A succesful attempt to rewrite my parser using monads
--All previous functionality exists except the symbol table
--that's for later

parse :: TokenList -> CST
parse tokens = do 
  let (tokenlist, cst) = statement (tokens, CST Statement [])
  addParentNode cst Program
  
statement :: TokenCST -> TokenCST  
([], _) = statementError 
statement ((token:rest), cst) =
  case (kind token) of
    PrintOp -> do
      let remaining = trace("Parsing print op") consumeToken ParenOpen (rest,cst)
      let follow = exper remaining
      consumeToken ParenClose follow
    ID -> do
      let remaining = trace("Parsing Id expression") consumeToken EqualsOp (rest,cst)
      exper remaining
    IntOp -> trace("Parsing int decleration") varDecl (rest,cst)
    CharOp -> trace("Parsing char decleration") varDecl (rest,cst)
    OpenBrace -> do
      let remaining = trace("Parsing statementList") statementList (rest,cst)
      consumeToken CloseBrace remaining
    _ -> unexpected token 

exper :: TokenCST -> TokenCST 
exper ([], _) = error("Error: Found nothing -- Expected digit, " ++
                 "string expression or ID in expr")
exper ((token:rest), cst) =
  case (kind token) of
    Digit -> intExper (rest,cst)
    CharacterList ->
      trace("Parsed character list") (rest,cst)
    ID -> trace("Parsed ID") (rest,cst)
    _ -> unexpected token

varDecl :: TokenCST -> TokenCST 
varDecl ([],_) = error("Error: Found nothing -- Expected ID in variable decleration")
varDecl list =
  trace("Parsing id in variable decleration") consumeToken ID list

statementList :: TokenCST -> TokenCST 
statementList ([],_) = statementError 
statementList ((token:rest),cst)
  --on finding follow of statementList epislon
  |tt == CloseBrace = ((token:rest),cst)
  --on finding first of statement do the statementList thing
  |tt == PrintOp || tt == ID || tt == IntOp || tt == CharOp || tt == OpenBrace = do
    let remaining = statement ((token:rest), cst)
    statementList remaining
  --otherwise you done screwed up
  |otherwise = unexpected token
  where tt = kind token 

intExper :: TokenCST -> TokenCST 
intExper ([],cst) = ([],cst)
intExper ((token:rest),cst) =
  case (kind token) of
    PlusOp -> exper (rest,cst)
    MinusOp -> exper (rest,cst) 
    --epislon in intExper because it is entered upon detection of a digit
    _ -> ((token:rest),cst)
