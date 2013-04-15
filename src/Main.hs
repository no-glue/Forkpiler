module Main where

import Lexer
import MParser
import SemanticAnalysis
import System.Environment

main = do
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let result = parse tokens
  let test = analyze result
  putStrLn(show result)
  putStrLn "done"

third (_,_,x) = x
second (_,x,_) = x
