module Main where

import Lexer
import MParser
import System.Environment

main = do
  [inFile] <- getArgs
  rawCode <- readFile inFile
  let tokens = Lexer.lex rawCode
  debugPrint tokens
  let result = parse tokens
  putStrLn(show result)
  putStrLn "done"

third (_,_,x) = x
second (_,x,_) = x
