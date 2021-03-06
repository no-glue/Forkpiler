{-# LANGUAGE BangPatterns #-}
module Lexer where

import Token
import Text.Regex.PCRE
import Data.List
import Debug.Trace

lex :: String -> TokenList
lex file = checkEnd $ (processFile fileLines 1)
  where
    fileLines = lines file

--checks the end to see if there is a $ and then tosses it out
--if there is one
checkEnd :: TokenList -> TokenList 
checkEnd [] = []
checkEnd tokens 
  |eof == -1 = trace("Warning no $ found. Adding it for you (Like a Boss)") tokens
  |(eof + 1)< size = trace("Warning code beyond $ found. It is being ignored" ++ show eof 
    ++ show size) take eof tokens
  |(eof+1) == size = take (eof) tokens
  |otherwise = error("unidentified lex error. most likely an error in the compiler uh oh!")
  where 
    eof = findEOF tokens
    size = length tokens

processFile :: [String] -> Int -> TokenList 
processFile [] _ = []
processFile (line:moreLines) x =
  processLine line x ++ processFile moreLines (x+1) 

processLine :: String -> Int -> TokenList 
processLine line lineNum = 
  let 
    brokenLine = respectQuotes $ words line 
  in foldr (\word next ->trace(word) $ processWord word lineNum ++ next) [] brokenLine
   
respectQuotes :: [String] -> [String] 
respectQuotes [] = []
respectQuotes (x:xs)
  |x =~ startQuote :: Bool = 
    let 
      quoted = takeWhile(inQuotes) (x:xs)
      concated = unwords quoted
    in concated : respectQuotes (quoted \\ (x:xs))
  |otherwise = x:respectQuotes xs
  where 
    word = "[a-z0-9]*"
    startQuote = "(^\")[a-z0-9 ]*(?!\")"
    endQuotes = "(!^\")[a-z0-9]*(\")"
    inQuotes y = ((y =~ word :: Bool) || (y =~ endQuotes :: Bool))


processWord :: String -> Int -> TokenList 
processWord [] _ = []
processWord input lineNum 
  |input =~ boolean :: Bool =
    let token = Token (input =~ boolean :: String) lineNum Boolean
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ true :: Bool =
    let token = Token (input =~ true :: String) lineNum TrueOp
    in token : processWord (input \\ (contents token)) lineNum 
  |input =~ false :: Bool =
    let token = Token (input =~ false :: String) lineNum FalseOp
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ characterList :: Bool =
    let token = Token (input =~ characterList :: String) lineNum CharacterList 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ brokenChList :: Bool =
    error("Error: Found missing \" at end of characterList:" ++ input)
  |input =~ parenOpen :: Bool = 
    let token = Token (input =~ parenOpen :: String)  lineNum ParenOpen 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ parenClose :: Bool = 
    let token = Token (input =~ parenClose :: String)  lineNum ParenClose 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ equality :: Bool =
    let token = Token (input =~ equality :: String) lineNum Equality
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ equalsOp :: Bool = 
    let token = Token (input =~ equalsOp :: String) lineNum EqualsOp 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ while :: Bool = 
    let token = Token (input =~ while :: String) lineNum While
    in token : processWord (input \\ (contents token )) lineNum
  |input =~ ifId :: Bool = 
    let token = Token (input =~ ifId :: String) lineNum If
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
  |input =~ longDigit :: Bool = 
    error("Error: Found a digit longer than one on integer" ++ (show lineNum) ++ 
      " with contents " ++ input)
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
  |input =~ longId :: Bool = 
    error("Error: Found character string outside of \"\":" ++ (show lineNum))
  |input =~ identifier :: Bool = 
    let token = Token (input =~ identifier :: String) lineNum ID
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ eof :: Bool = 
    let token = Token (input =~ eof :: String) lineNum EOF 
    in token : processWord (input \\ (contents token)) lineNum
  |otherwise = 
    error("Error: Unexpected character in lex:"++input)
  where 
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equality = "^=="
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
    digit = "^[0-9](?![a-z])"
    longDigit = "^[0-9]{2,}(?![a-z])"
    characterList = "^[\"][a-z ]*\""
    brokenChList = "^[\"][a-z]*(?!\")"
    printOp = "^(print|P)"
    int = "^int"
    char = "^string"
    identifier = "^[a-z](?![1-9])"
    longId ="^[a-z]{2,}(?![1-9])"
    while = "^while"
    ifId = "^if"
    boolean = "^boolean"
    true = "^true"
    false = "^false"
    eof = "^\\$"

debugPrint :: [Token] -> IO ()
debugPrint [] = putStrLn "Done Lexing (Like a Boss)" 
debugPrint (x:xs)
  | kind x == Error = 
    error("lex error" ++ (show $ kind x) ++ " " ++ (contents x) ++ " at line " 
      ++ (show (location x)))  
  |otherwise = do 
    putStrLn ("lexing " ++ (contents x) ++ " as " ++ (show $ kind x) ++ "(Like a Boss)")
    debugPrint xs 

findEOF :: TokenList -> Int
findEOF x = findToken x EOF
