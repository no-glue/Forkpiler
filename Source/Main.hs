module Main where

import Lexer
import MParser
import System.Environment

main = do
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  statement tokens 
  --putStrLn ("Errors? " ++ (show (third parseOutput)))
  --putStrLn ("symbol table " ++ (show (second parseOutput)))
  writeFile "output.txt" (show tokens) 

third (_,_,x) = x
second (_,x,_) = x
