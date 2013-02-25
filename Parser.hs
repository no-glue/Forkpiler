module Parser where

import Token

parse :: [Token] -> Bool
parse [] = error("parsing program at start. program appears empty") 
parse tokens = statement tokens

statement :: [Token] -> Bool
statement (token:next:rest)
  |checkToken token "print"= 
    requireToken next "parenOpen" . exper rest location $ token
  |checkToken token "id"= 
    requireToken next "equalsOp" . exper rest location $ token
  |kind token == "openBrace" = statementList next:rest
  |otherwise = vardecl

--exper :: [Token] -> Int -> [Token]
exper [] line = error("parsing expression near nothing found at line:" ++ line)
exper [] _ = error("paring expression and for some reason I can't be more helpful")
exper (token:next:rest) line
  |kind token == "digit" = intExper next:rest
  |kind token == "charList" = requireToken next "parenClose"  
  |kind token == "id" = True 
  |otherwise = error("parsing expresion near line" ++ line)

--statementList :: [Token] -> Int -> [Token]
statementList [] line = error("parsing statementList nothing found at line:" ++ line)
statementList [] _ = error("parsing statementList and for some reason I can't be more helpful")
statementList (token:rest) _ 
  |not checkToken token "closeBrace" = statement token:rest 
  |otherwise = True 

vardecl (token:next:rest)
  |type' token = requireToken next "id"
  |otherwise = error("parse error in variable declaration at line:" ++ location token) 

type' (token:rest) 
  |checkToken token "int" = True
  |checkToken token "char" = True
  |otherwise = error("parse error in type decleration. expecting type found " ++ kind token
    ++ " on line " ++ location token) 

intExper (token:next:rest)
  |checkToken token "digit" && op next = exper next
  |checkToken token "digit" = True
  |otherwise = error("parse error in int expression. expecting digit or op and found " ++
    kind token ++ " on line " ++ location token)

op (token:rest)
  |checkToken token "plusOp" = True
  |checkToken token "minusOp" = True
  |otherwise = error("parse error in int expression. expectiong op and found " ++
    kind token ++ " on line " ++ location token)

--checkToken :: Token -> String -> Bool
checkToken token string 
  |kind token  == string = True
  |otherwise = False

--requireToken :: Token -> String -> Bool
requireToken token string
  |kind token == string = True 
  |otherwise = error("parse error. expecting " ++ string ++ " got " ++ kind token
    ++ " on line " ++ location token)
