module Lexer where

import Token
import Text.Regex.PCRE
import Data.List

lex :: String -> [Token] 
lex =
  checkEnd . processFile

checkEnd :: [Token] -> [Token]
checkEnd [] = []
checkEnd tokens 
  |kind end == kind dollarToken = tokens
  |otherwise = Token "No $ found added $" 1 "lex warning" : tokens 
  where 
    end = last tokens
    dollarToken = Token "$" 1 "eof"

processFile :: String -> [Token]
processFile file = 
  let fileLines = lines file
  in foldr (\ line next -> processLine line (numLines - length next) ++ next) [] fileLines
  where
    numLines = length file

processLine :: String -> Int -> [Token]
processLine line lineNum = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word lineNum ++ next) [] brokenLine

processWord :: String -> Int -> [Token]
processWord [] x = []
processWord input lineNum 
  |input =~ characterList :: Bool =
    let token = Token (input =~ characterList :: String) lineNum "charList"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String)  lineNum "parenOpen"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String)  lineNum "parenClose"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) lineNum "equalsOp"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) lineNum "plusOp"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) lineNum "minusOp" 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) lineNum "openBrace" 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) lineNum "closeBrace" 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ digit :: Bool =
    let token = Token (input =~ digit :: String) lineNum "digit"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ printOp :: Bool = 
    let token = Token (input =~ printOp :: String) lineNum "print"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ int :: Bool = 
    let token = Token (input =~ int :: String) lineNum "int"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ char :: Bool = 
    let token = Token (input =~ char :: String) lineNum "char"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ identifier :: Bool = 
    let token = Token (input =~ identifier :: String) lineNum "id"
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ eof :: Bool = 
    let token = Token (input =~ eof :: String) lineNum "eof"
    in token : processWord (input \\ (contents token)) lineNum
  |otherwise = 
    let token = Token input lineNum "lex error"
    in token : [] 
  where 
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
    digit = "^[1-9]+(?![A-Za-z])"
    characterList = "^[\"][a-zA-Z]*\""
    printOp = "^(Print|P)"
    int = "^int"
    char = "^char"
    identifier = "^[a-zA-Z]$"
    eof = "^\\$$"

debugPrint :: [Token] -> IO ()
debugPrint [] = putStrLn "Done Lexing (Like a Boss)" 
debugPrint (x:xs) = do
  putStrLn ("lexing " ++ (contents x) ++ " as " ++ (kind x) ++ "(Like a Boss)") 
  debugPrint xs 
