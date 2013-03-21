module MParser where

import Token
import Debug.Trace
import ParserHelpers

--An attempt to rewrite my parser using monads

parse :: TokenList -> TokenList
parse tokens = empty $ statement tokens
  
statement :: TokenList -> TokenList
statement [] = statementError 
statement (token:rest) =
  case (kind token) of
    PrintOp -> do
      let remaining = trace("Parsing print op") consumeToken ParenOpen rest
      let follow = exper remaining
      consumeToken ParenClose follow
    ID -> do
      let remaining = trace("Parsing Id expression") consumeToken EqualsOp rest
      exper remaining
    IntOp -> trace("Parsing int decleration") varDecl rest
    CharOp -> trace("Parsing char decleration") varDecl rest
    OpenBrace -> do
      let remaining = trace("Parsing statementList") statementList rest
      consumeToken CloseBrace remaining
    _ -> unexpected token 

exper :: TokenList -> TokenList
exper [] = error("Error: Found nothing -- Expected digit, " ++
                 "string expression or ID in expr")
exper (token:rest) =
  case (kind token) of
    Digit -> intExper rest 
    CharacterList ->
      trace("Parsed character list") rest
    ID -> trace("Parsed ID") rest
    _ -> unexpected token

varDecl :: TokenList -> TokenList
varDecl [] = error("Error: Found nothing -- Expected ID in variable decleration")
varDecl list =
  trace("Parsing id in variable decleration") consumeToken ID list

statementList :: TokenList -> TokenList
statementList [] = statementError 
statementList (token:rest)
  --on finding follow of statementList epislon
  |tt == CloseBrace = token:rest 
  --on finding first of statement do the statementList thing
  |tt == PrintOp || tt == ID || tt == IntOp || tt == CharOp || tt == OpenBrace = do
    let remaining = statement (token:rest)
    statementList remaining
  --otherwise you done screwed up
  |otherwise = unexpected token
  where tt = kind token 

intExper :: TokenList -> TokenList
intExper [] = []
intExper (token:rest) =
  case (kind token) of
    PlusOp -> exper rest
    MinusOp -> exper rest
    --epislon in intExper because it is entered upon detection of a digit
    _ -> token:rest 
