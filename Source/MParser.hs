module MParser where

import Token
import Debug.Trace

--An attempt to rewrite my parser using monads

parse :: TokenList -> TokenList
parse tokens = statement tokens
  
statement :: TokenList -> TokenList
statement [] = error("Error: Found nothing -- Expected " ++
                     "print, ID, type, or { in statement")
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
    _ -> error("Error: Unexpected " ++ (show $ kind token) ++ " in statement on line " ++
               (show $ location token)) 

exper :: TokenList -> TokenList
exper [] = error("Error: Found nothing -- Expected digit, " ++
                 "string expression or ID in expr")
exper (token:rest) =
  case (kind token) of
    Digit -> do
      let remaining = intExper rest
      exper remaining
    CharacterList ->
      trace("Parsed character list") rest
    ID -> trace("Parsed ID") rest
    _ ->
      error("Error: Unexpected " ++ (show $ kind token) ++ " in expr on line " ++
            (show $ location token))

varDecl :: TokenList -> TokenList
varDecl [] = error("Error: Found nothing -- Expected ID in variable decleration")
varDecl list =
  trace("Parsing id in variable decleration") consumeToken ID list

statementList :: TokenList -> TokenList
statementList [] = error("Error: Found nothing -- Expected print, " ++
                         "ID, type, or { at start of statementList")
statementList (token:rest)
  --on finding follow of statementList epislon
  |tt == CloseBrace = token:rest 
  --on finding first of statement do the statementList thing
  |tt == PrintOp || tt == ID || tt == IntOp || tt == CharOp || tt == OpenBrace = do
    let remaining = statement (token:rest)
    statementList remaining
  --otherwise you done screwed up
  |otherwise = error("Error: Unexpected " ++ (show $ tt) ++ " in statementList " ++
                     "on line " ++ (show $ location token))
  where tt = kind token 

intExper :: TokenList -> TokenList
intExper [] = error("Error: Found nothing -- Expected opperator in intExpr")
intExper (token:rest) =
  case (kind token) of
    PlusOp -> exper rest
    MinusOp -> exper rest
    --epislon in intExper because it is entered upon detection of a digit
    _ -> token:rest 

consumeToken :: TokenType -> TokenList -> TokenList
consumeToken typi [] = error("Error: Found nothing -- Expected " 
              ++ show typi) 
consumeToken typi (x:xs) = 
  if kind x == typi
  then trace("consuming " ++ show typi ) xs
  else error("Error: Found " ++ (show $ kind x) ++ " -- Expected " 
              ++ show typi ++ " On line " ++ (show $ location x))

head' :: TokenList -> Token
head' (x:_) = x
head' [] = Token "empty" (-1) EOF 
