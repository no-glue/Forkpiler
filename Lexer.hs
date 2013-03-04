module Lexer where

import Token
import Text.Regex.PCRE
import Data.List
import Debug.Trace

lex :: String -> TokenList
lex = checkEnd . processFile

--checks the end to see if there is a $ and then tosses it out
-- if there is one
checkEnd :: TokenList -> TokenList 
checkEnd [] = []
checkEnd tokens 
  |eof == -1 = trace("Warning no $ found. Adding it for you (Like a Boss)") tokens
  |(eof + 1)< size = trace("Warning code beyond $ found. It is being ignored" ++ show eof ++ show size)
                         take eof tokens
  |(eof+1) == size = take (eof) tokens
  |otherwise = error("unidentified lex error. most likely an error in the compiler uh oh!")
  where 
    eof = findEOF tokens
    size = length tokens

processFile :: String -> TokenList 
processFile file =
  foldr (folding)  [] fileLines
  where
    fileLines = lines file
    numLines = length fileLines
    folding = (\ line next -> processLine line (numLines - length next) ++ next) 

processLine :: String -> Int -> TokenList 
processLine line lineNum = 
  let brokenLine = words line
  in foldr (\ word next -> processWord word lineNum ++ next) [] brokenLine

processWord :: String -> Int -> TokenList 
processWord [] _ = []
processWord input lineNum 
  |input =~ characterList :: Bool =
    let token = Token (input =~ characterList :: String) lineNum CharacterList 
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ brokenChList :: Bool =
    error("Found  missing \" at end of characterList:" ++ input)
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
  |input =~ longId :: Bool = 
    error("Found character string outside of \"\":" ++ (show lineNum))
  |input =~ identifier :: Bool = 
    let token = Token (input =~ identifier :: String) lineNum ID
    in token : processWord (input \\ (contents token)) lineNum
  |input =~ eof :: Bool = 
    let token = Token (input =~ eof :: String) lineNum EOF 
    in token : processWord (input \\ (contents token)) lineNum
  |otherwise = 
    error("Unexpected character in lex:"++input)
  where 
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
    digit = "^[0-9]+(?![a-z])"
    characterList = "^[\"][a-z]*\""
    brokenChList = "^[\"][a-z]*(?!\")"
    printOp = "^(print|P)"
    int = "^int"
    char = "^char"
    identifier = "^[a-z](?![1-9])"
    longId ="^[a-z]{2,}(?![1-9])"
    eof = "^\\$"

debugPrint :: [Token] -> IO ()
debugPrint [] = putStrLn "Done Lexing (Like a Boss)" 
debugPrint (x:xs)
  | kind x == Error = 
    error("lex error" ++ (show $ kind x) ++ " " ++ (contents x) ++ " at line " ++ (show (location x)))  
  |otherwise = do 
    putStrLn ("lexing " ++ (contents x) ++ " as " ++ (show $ kind x) ++ "(Like a Boss)")
    debugPrint xs 

findEOF :: TokenList -> Int
findEOF x = findToken x EOF

fixLineNumbers :: TokenList -> TokenList
fixLineNumbers [] = []
fixLineNumbers (x:xs)
  |location x < 0 =
    let token = Token (contents x) 0 (kind x)
    in token : fixLineNumbers xs
  |otherwise = x:xs
