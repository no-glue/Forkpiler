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
processWord [] = []
processWord input 
  |input =~ quotation :: Bool =
    let token = Token (input =~ quotation :: String) (0,0) "quotation" 
    in (processWord (input \\ (contents token))) 
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String) (0,0) "parenOpen" 
    in token : (processWord (input \\ (contents token))) 
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String) (0,0) "parenClose" 
    in token : (processWord (input \\ (contents token)))
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) (0,0) "equalsOp" 
    in token : (processWord (input \\ (contents token)))
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) (0,0) "plusOp" 
    in token : (processWord (input \\ (contents token)))
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) (0,0) "minusOp" 
    in token : (processWord (input \\ (contents token)))
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) (0,0) "openBrace" 
    in token : (processWord (input \\ (contents token)))
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) (0,0) "closeBrace" 
    in token : (processWord (input \\ (contents token)))
  |input =~ digit :: Bool =
    let token = Token (input =~ digit :: String) (0,0) "digit"
    in token : (processWord (input \\ (contents token)))
  |otherwise = error("you done fucked up!")
  where 
    quotation = "^[\"]"
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
    digit = "^[1-9]*"
