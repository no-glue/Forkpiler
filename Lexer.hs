module Lexer where

import Token
import Text.Regex.PCRE
import Data.List
import Debug.Trace

lex :: String -> [Token] 
lex = checkEnd . processFile

checkEnd :: [Token] -> [Token]
checkEnd [] = []
checkEnd tokens 
  |kind end == kind dollarToken = tokens
  |otherwise = Token "No $ found added $" 1 Warning : tokens 
  where 
    end = last tokens
    dollarToken = Token "$" 1 EOF 

processFile :: String -> [Token]
processFile file = 
  foldr (folding)  [] fileLines
  where
    fileLines = lines file
    numLines = length fileLines
    folding =(\ line next -> processLine line (numLines - trace( "next:" ++ show (length next)) length next) ++ next) 

processLine :: String -> Int -> [Token]
processLine line lineNum = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word lineNum ++ next) [] brokenLine

processWord :: String -> Int -> [Token]
processWord [] x = []
processWord input lineNum 
  |input =~ characterList :: Bool =
    let token = Token (input =~ characterList :: String) lineNum CharacterList 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String)  lineNum ParenOpen 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String)  lineNum ParenClose 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) lineNum EqualsOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) lineNum PlusOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) lineNum MinusOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) lineNum OpenBrace 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) lineNum CloseBrace 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ digit :: Bool =
    let token = Token (input =~ digit :: String) lineNum Digit 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ printOp :: Bool = 
    let token = Token (input =~ printOp :: String) lineNum PrintOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ int :: Bool = 
    let token = Token (input =~ int :: String) lineNum IntOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ char :: Bool = 
    let token = Token (input =~ char :: String) lineNum CharOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ identifier :: Bool = 
    let token = Token (input =~ identifier :: String) lineNum ID
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ eof :: Bool = 
    let token = Token (input =~ eof :: String) lineNum EOF 
    in token : processWord (input \\ (contents token)) lineNum
  |otherwise = 
    let token = Token input lineNum Error 
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
debugPrint (x:xs)
  | kind x == Error = 
    error((show $ kind x) ++ " " ++ (contents x) ++ " at line " ++ (show (location x)))  
  |otherwise = do 
    putStrLn ("lexing " ++ (contents x) ++ " as " ++ (show $ kind x) ++ "(Like a Boss)")
    debugPrint xs 
