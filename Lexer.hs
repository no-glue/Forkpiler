Module Lexer where

import Token
import Text.Regex.Posix
import Data.List

lex :: String -> [Token]
lex input = do
  breakLines = lines input
  foldl (\++) [] (foldl (++) []  

processLine :: [String] -> [Token]
processLine line = 
  foldl (\++) [] 

processWord :: String -> Int -> [Token]
processWord input 
  |input =~ quotation :: Bool =
    let token = Token (input =~ quotation :: String) (lineNumber,0) "quotation" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String) (lineNumber,0) "parenOpen" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String) (lineNumber,0) "parenClose" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) (lineNumber,0) "equalsOp" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) (lineNumber,0) "plusOp" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) (lineNumber,0) "minusOp" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) (lineNumber,0) "openBrace" 
      in token : lexer (delete (contents token) input) lineNumber
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) (lineNumber,0) "closeBrace" 
      in token : lexer (delete (contents token) input) lineNumber
  where 
    whitespace = "^[ \t\n]"
    endline = "^[\n]"
    quotation = "^[\"]"
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
