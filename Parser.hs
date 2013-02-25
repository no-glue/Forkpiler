module Parser where

parse :: [Token]
parse [] = error("parsing program at start. program appears empty") 
parse tokens = statement

statement :: [Token]
statement (token:next:rest)
  |checkToken token "print" && checkToken next "parenOpen" =
    exper rest location $ token
  |kind token == "id" && kind next == "equalsOp" =
    exper rest
  |kind token == "openBrace" = statementList next:rest
  |otherwise = vardecl

exper :: [Token] -> Int -> [Token]
exper [] line = error("parsing expression near line:" ++ line)
exper [] = error("paring expression and for some reason I can't be more helpful")
exper (token:rest)
  |kind token == "digit" = digitExper rest
  |kind token == "charList" = []
  |kind token == "id" = []
  |otherwise = error("parsing expresion near line" ++ line)

statementList :: [Token] -> Int -> [Token]
statementList [] line = error("parsing statementList near line" ++ line)
statementList [] = error("parsing statementList and for some reason I can't be more helpful")
statementList 

checkToken :: Token -> String -> Bool
checkToken token string 
  |kind token == string = True
  |otherwise = false

requireToken :: Token -> String
requireToken token string
  |kind token == string 
  |otherwise = error("parse error. expecting " ++ string ++ " got " ++ kind token
    ++ " on line " ++ location token)
