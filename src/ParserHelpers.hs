module ParserHelpers where

import Token
import AST
import Debug.Trace

consumeTokenAsChild :: TokenType -> TokenAST -> TokenAST
consumeTokenAsChild typi ([], _) = error("Error: Found nothing -- Expected " 
              ++ show typi) 
consumeTokenAsChild typi ((x:xs), cst) = 
  if kind x == typi
  then trace("consuming " ++ show typi ) (xs, addChildNode cst (Terminal x))
  else error("Error: Found " ++ (show $ kind x) ++ " -- Expected " 
              ++ show typi ++ " On line " ++ (show $ location x))

consumeTokenAsParent :: TokenType -> TokenAST -> TokenAST
consumeTokenAsParent typi ([], _) = error("Error: Found nothing -- Expected " 
              ++ show typi) 
consumeTokenAsParent typi ((x:xs), cst) = 
  if kind x == typi
  then trace("consuming " ++ show typi ) (xs, addParentNode cst (Terminal x))
  else error("Error: Found " ++ (show $ kind x) ++ " -- Expected " 
              ++ show typi ++ " On line " ++ (show $ location x))

--throws an error. I have no idea what type this would be
unexpected token = 
  error("Error: Unexpected " ++ printKind token ++ " in expression " ++
        "on line " ++ printLocation token) 

statementError = error("Error: Found nothing -- Expected print, ID, type, or { " ++
  "in Statement. Possiable dangling {")

empty :: TokenList -> TokenList
empty [] = []
empty (token:_)
  |tt == CloseBrace = error("Found closeBrace without " ++
    "matching openBrace on line " ++ printLocation token)
  |tt == ParenClose = error("Found closeParen without " ++
    "matching open paren on line " ++ printLocation token) 
  |tt == Digit = error("Found unexpected digit on line " ++ 
    printLocation token ++ " most likely began math with an ID")
  |otherwise = error("More than one statement found outside of {} on line." ++ 
    printLocation token ++ " Maybe missing {}?")
  where tt = kind token
