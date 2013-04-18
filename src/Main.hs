{-# LANGUAGE BangPatterns #-}
module Main where

import Lexer
import MParser
import SemanticAnalysis
import System.Environment
import AST
import Data.Map as Map 
import SymbolTable
main = do
  --readIORef  ref >>= print 
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let !ast = parse tokens
 -- let test = analyze result
  --putStrLn (show ast)
  let !(symboltable,newAst) = buildSymbolTable ast
  --putStrLn (show symboltable)
  --putStrLn(show newAst)
  let !dummySymbol = typeCheck newAst symboltable 0
  let updatedSymbolTable = updateSymbolTable newAst symboltable (0,0) 
  putStrLn (show newAst)
  putStrLn(show updatedSymbolTable)
  putStrLn "done"

third (_,_,x) = x
second (_,x,_) = x
