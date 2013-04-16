module Main where

import Lexer
import MParser
import SemanticAnalysis
import System.Environment
import AST

main = do
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let ast = parse tokens
 -- let test = analyze result
  putStrLn (show ast)
  let symboltable = buildSymbolTable ast
  putStrLn (show symboltable)
  putStrLn "done"

third (_,_,x) = x
second (_,x,_) = x
