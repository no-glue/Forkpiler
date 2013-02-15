module Lexer where

import Token
import Text.Regex.Posix
import Data.List

lex :: String -> [Token]
lex input =
  let breakLines = lines input
  in foldr (\ line next -> processLine line ++ next) [] breakLines   

processLine :: String -> [Token]
processLine line = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word ++ next) [] brokenLine

processWord :: String -> [Token]
processWord input 
  |input =~ quotation :: Bool =
    let token = Token (input =~ quotation :: String) (0,0) "quotation" 
    in (processWord (input \\ (contents token))) 
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String) (0,0) "parenOpen" 
    in token : (processWord (input \\ (contents token))) 
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String) (0,0) "parenClose" 
    in (processWord (input \\ (contents token)))
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) (0,0) "equalsOp" 
    in (processWord (input \\ (contents token)))
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) (0,0) "plusOp" 
    in (processWord (input \\ (contents token)))
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) (0,0) "minusOp" 
    in (processWord (input \\ (contents token)))
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) (0,0) "openBrace" 
    in (processWord (input \\ (contents token)))
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) (0,0) "closeBrace" 
    in (processWord (input \\ (contents token)))
  where 
    quotation = "^[\"]"
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
