module Lexer where

import Token
import Text.Regex.Posix

lex :: String -> [Token]
lex input =
  foldr (\ line next -> processLine line ++ next) [] breakLines   
  where breakLines = lines input

processLine :: String -> [Token]
processLine line = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word : next) [] brokenLine

processWord :: String -> Token
processWord input 
  |input =~ quotation :: Bool =
    Token (input =~ quotation :: String) (0,0) "quotation" 
  |input =~ parenOpen :: Bool = 
    Token (input =~ parenOpen :: String) (0,0) "parenOpen" 
  |input =~ parenClose :: Bool = 
    Token (input =~ parenClose :: String) (0,0) "parenClose" 
  |input =~ equalsOp :: Bool = 
    Token (input =~ equalsOp :: String) (0,0) "equalsOp" 
  |input =~ plusOp :: Bool = 
    Token (input =~ plusOp :: String) (0,0) "plusOp" 
  |input =~ minusOp :: Bool =
    Token (input =~ minusOp :: String) (0,0) "minusOp" 
  |input =~ openBrace :: Bool =
    Token (input =~ openBrace :: String) (0,0) "openBrace" 
  |input =~ closeBrace :: Bool =
    Token (input =~ closeBrace :: String) (0,0) "closeBrace" 
  where 
    quotation = "^[\"]"
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
