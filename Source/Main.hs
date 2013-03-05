module Main where

import Lexer
import Parser
import System.Environment

main = do
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let parseOutput = parse tokens 
  putStrLn ("Errors? " ++ (show (third parseOutput)))
  putStrLn ("symbol table " ++ (show (second parseOutput)))
  writeFile "output.txt" (show tokens) 

third (_,_,x) = x
second (_,x,_) = x