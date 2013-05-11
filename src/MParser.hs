{-# LANGUAGE BangPatterns #-}
module MParser where

import Token
import AST
import Debug.Trace
import ParserHelpers

--A succesful attempt to rewrite my parser using monads
--All previous functionality exists except the symbol table
--that's for later

parse :: TokenList -> AST
parse tokens =
  let 
    (tokenlist, ast) = statement tokens
    !test = empty tokenlist
    in ast
  
statement :: TokenList -> TokenAST  
[] = statementError 
statement (token:rest)
    |tt == PrintOp =
      let
        remaining = trace("Parsing print op")  consumeToken ParenOpen rest
        (follow, experTree) = exper remaining
        !consumed = consumeToken ParenClose follow
      in (consumed ,addChildTree ast experTree)
    |tt == ID =  
      let 
        (remaining, ast2) = trace("Parsing Id expression") 
          consumeTokenAsParent EqualsOp (rest,ast)
        (expression, child) = exper remaining
      in (expression, addChildTree ast2 child)
    |tt == IntOp || tt == CharOp || tt == Boolean =
      let (decleration, child) = trace("Parsing decleration") varDecl rest
      in  (decleration, addChildTree ast child)
    |tt == While || tt == If =
      let
        (block,boolChild) = trace("Parsing while") booleanExpression rest
        (remaining, blockChild) = statement block
        whileBool = addChildTree ast boolChild
      in (remaining, addChildTree whileBool blockChild)
    |tt == OpenBrace =
      let 
        (remaining, ast2) = trace("Parsing statementList") statementList (rest,ast)
        !consumed = consumeToken CloseBrace remaining
      in (consumed, ast2) 
    |otherwise = unexpected token
    where
      ast = AST (newNode token) []
      tt = kind token

exper :: TokenList -> TokenAST 
exper [] = error("Error: Found nothing -- Expected digit, " ++
                 "string expression or ID in expr")
exper (token:rest)
    |tt == Digit = intExper (rest,ast)
    |tt == CharacterList = trace("Parsed character list") (rest,ast)
    |tt == ID = trace("Parsed ID") (rest,ast)
    |tt == TrueOp || tt == FalseOp || tt == ParenOpen = trace "parsing boolean experession" 
      booleanExpression (token:rest)
    |otherwise = unexpected token
    where ast = AST (newNode token) []
          tt = kind token

varDecl :: TokenList -> TokenAST 
varDecl [] = error("Error: Found nothing -- Expected ID in variable decleration")
varDecl (token:rest)
  |tt == ID = trace("Parsing id in variable decleration") (rest, ast) 
  |otherwise = unexpected token
  where ast = AST (newNode token) []
        tt = kind token

statementList :: TokenAST -> TokenAST
statementList ([],_) = statementError 
statementList ((token:rest),ast)
  --on finding follow of statementList epislon
  |tt == CloseBrace = ((token:rest),ast)
  --on finding first of statement do the statementList thing
  |tt == PrintOp || tt == ID || tt == IntOp || tt == CharOp || tt == Boolean || tt == If
    || tt == While || tt == OpenBrace = 
    let (list, ast2) = statement (token:rest)
    in statementList (list, addChildTree ast ast2)
  --otherwise you done screwed up
  |otherwise = unexpected token
  where tt = kind token

intExper :: TokenAST -> TokenAST 
intExper ([], ast) = ([], ast)
intExper ((token:rest), ast) = 
    case (kind token) of
      PlusOp  ->
         let (expression, child) = exper rest
         in (expression, addChildTree parent child)
      MinusOp ->
         let (expression, child) = exper rest
         in (expression, addChildTree parent child)
    --epislon in intExper because it is entered upon detection of a digit
      _ ->  ((token:rest),ast)
    where tt = kind token 
          parent = addChildTree (AST (newNode token) []) ast

booleanExpression :: TokenList -> TokenAST
booleanExpression [] = error "Error: Found nothing -- Expected Boolean Expression"
booleanExpression (x:xs)
  |tt == ParenOpen = 
    let
      (rest, leftChild) = trace "Parsing left expression in boolean expression" exper xs
      (leftSide, parent) = consumeTokenAsParent Equality (rest, leftChild)
      (remaining, rightChild) = trace "Parsing right expression in boolean expression" exper leftSide
      final = consumeToken ParenClose remaining
    in (final, addChildTree parent rightChild)
  |tt == FalseOp || tt == TrueOp = (xs, AST (newNode x) [])
  |otherwise = unexpected x
  where tt = kind x
