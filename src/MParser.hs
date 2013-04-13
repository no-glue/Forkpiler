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
      let (remaining, cst3) = trace("Parsing print op") consumeToken ParenOpen (rest,cst2)
      let (follow, child) = exper (remaining, CST Expr [])
      consumeToken ParenClose (follow, addChildTree cst3 child)
    ID -> do
      let (remaining, cst3) = trace("Parsing Id expression") consumeToken EqualsOp (rest,cst2)
      let (expression, child) = exper (remaining, CST Expr []) 
      trace(show child) (expression, addChildTree cst3 child)
    IntOp -> do
      let (decleration, tree) = trace("Parsing int decleration") varDecl (rest, CST terminal [])
      (decleration, addChildTree cst tree)
    CharOp -> do     
      let (decleration, tree) = trace("Parsing char decleration") varDecl (rest, CST terminal [])
      (decleration, addChildTree cst tree)
    OpenBrace -> do
      let remaining = trace("Parsing statementList") statementList (rest,cst2)
      consumeToken CloseBrace remaining
    _ -> unexpected token 
    where cst2 = addChildNode cst terminal 
          terminal = Terminal token

exper :: TokenCST -> TokenCST 
exper ([], _) = error("Error: Found nothing -- Expected digit, " ++
                 "string expression or ID in expr")
exper ((token:rest), cst) =
  case (kind token) of
    Digit -> intExper (rest,cst2)
    CharacterList ->
      trace("Parsed character list") (rest,cst2)
    ID -> trace("Parsed ID") (rest,cst2)
    _ -> unexpected token
    where cst2 = addChildNode cst (Terminal token)

varDecl :: TokenCST -> TokenCST 
varDecl ([],_) = error("Error: Found nothing -- Expected ID in variable decleration")
varDecl list =
  trace("Parsing id in variable decleration") consumeToken ID list

statementList :: TokenCST -> TokenCST 
statementList ([],_) = statementError 
statementList ((token:rest),cst)
  --on finding follow of statementList epislon
  |tt == CloseBrace = ((token:rest),cst2)
  --on finding first of statement do the statementList thing
  |tt == PrintOp || tt == ID || tt == IntOp || tt == CharOp || tt == OpenBrace = do
    let remaining = statement ((token:rest), cst2)
    statementList remaining
  --otherwise you done screwed up
  |otherwise = unexpected token
  where tt = kind token 
        cst2 = addChildNode cst (Terminal token)

intExper :: TokenCST -> TokenCST 
intExper ([],cst) = ([],cst)
intExper ((token:rest),cst) =
  case (kind token) of
    PlusOp -> exper (rest,cst2)
    MinusOp -> exper (rest,cst2) 
    --epislon in intExper because it is entered upon detection of a digit
    _ -> ((token:rest),cst)
    where cst2 = addChildNode cst (Terminal token)
