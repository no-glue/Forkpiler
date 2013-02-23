module Lexer where

import Token
import Text.Regex.PCRE
import Data.List

lex :: String -> [(Token,Token)]
lex =
  checkEnd . processFile

checkEnd :: [(Token,Token)] -> [(Token,Token)]
checkEnd [] = []
checkEnd tokens 
  |kind end == kind dollarToken = tokens
  |otherwise = (Token "No $ found added $" (0,0) "lex warning", 
               dollarToken ) : tokens 
  where 
    end = fst (last tokens)
    dollarToken = Token "$" (0,0) "eof"

processFile :: String -> [(Token,Token)]
processFile file = 
  let fileLines = lines file
  in foldr (\ line next -> processLine line ++ next) [] fileLines

processLine :: String -> [(Token,Token)]
processLine line = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word ++ next) [] brokenLine

processWord :: String -> [(Token,Token)]
processWord [] = []
processWord input 
  |input =~ characterList :: Bool =
    let token = Token (input =~ characterList :: String) (0,0) "charList"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String) (0,0) "parenOpen"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String) (0,0) "parenClose"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) (0,0) "equalsOp"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ plusOp :: Bool = 
    let token = Token (input =~ plusOp :: String) (0,0) "plusOp"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ minusOp :: Bool =
    let token = Token (input =~ minusOp :: String) (0,0) "minusOp" 
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ openBrace :: Bool =
    let token = Token (input =~ openBrace :: String) (0,0) "openBrace" 
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ closeBrace :: Bool =
    let token = Token (input =~ closeBrace :: String) (0,0) "closeBrace" 
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ digit :: Bool =
    let token = Token (input =~ digit :: String) (0,0) "digit"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ printOp :: Bool = 
    let token = Token (input =~ printOp :: String) (0,0) "print"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ int :: Bool = 
    let token = Token (input =~ int :: String) (0,0) "int"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ char :: Bool = 
    let token = Token (input =~ char :: String) (0,0) "char"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ identifier :: Bool = 
    let token = Token (input =~ identifier :: String) (0,0) "id"
    in buildTokens token : processWord (input \\ (contents token))
  |input =~ eof :: Bool = 
    let token = Token (input =~ eof :: String) (0,0) "eof"
    in buildTokens token : processWord (input \\ (contents token))
  |otherwise = 
    let token = Token input (0,0) "lex error"
    in buildTokens token : [] 
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

--debugPrint :: Token -> Token
debugPrint (x:xs) = do
  putStrLn ("lexing " ++ (contents x) ++ " as " ++ (kind x)) 
  debugPrint xs 
