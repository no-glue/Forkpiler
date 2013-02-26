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
  putStrLn (show parseOutput ++ "shouldn't have anything before me otherwise there was an undetected parse error")
  writeFile "output.txt" (show tokens) 
