{-# LANGUAGE BangPatterns #-}
module Main where

import Lexer
import MParser
import SemanticAnalysis
import System.Environment
import AST
import Data.Map as Map 
import SymbolTable
import CodeGen
import Backpatch

main = do
  --readIORef  ref >>= print 
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let !ast = parse tokens
 -- let test = analyze result
  --putStrLn (show ast)
  let !(newAst,symboltable) = buildSymbolTable ast
  putStrLn "\n"
  putStrLn $ show newAst
  let !dummySymbol = typeCheck newAst symboltable 0
  putStrLn $ show dummySymbol
  --let updatedSymbolTable = updateSymbolTable newAst symboltable (0,0)
  putStrLn $ prettyPrint symboltable
  --let warnings1 = warnUsedButUnintilized updatedSymbolTable
  --let warnings2 = warnDecleredButUnUsed updatedSymbolTable
  --putStrLn ("WARNINGS: Used but uninitilized: " ++ show warnings1)
  --putStrLn ("WARNINGS: Unused but Declered: " ++ show warnings2)
  let code = words $ "EA EA " ++ codeGen symboltable newAst 0 ++ " 00 00 "
  putStrLn $ show code
  let backPatchedCode = backpatch (symboltable, Map.empty) code (length code)
  putStrLn backPatchedCode

third (_,_,x) = x
second (_,x,_) = x
