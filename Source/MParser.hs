module MParser where

import Token
import Debug.Trace

--An attempt to rewrite my parser using monads

parse :: TokenList -> TokenList
parse tokens = statement tokens
  
statement :: TokenList -> TokenList
statement (token:rest) =
  case (kind token) of
    PrintOp -> do
      let remaining = trace("Parsing print op") consumeToken ParenOpen rest
      let follow = exper remaining
      consumeToken ParenClose follow
    ID -> do
      let remaining = trace("Parsing id") consumeToken EqualsOP rest
    _ -> error("Error found nothing")

exper :: TokenList -> TokenList
exper (token:rest) =
  case (kind token) of
    CharacterList ->
      trace("Parsed character list") rest
    _ ->
      error("not implemented")

consumeToken :: TokenType -> TokenList -> TokenList
consumeToken typi [] = error("Error: Found nothing -- Expected " 
              ++ show typi) 
consumeToken typi (x:xs) = if kind x == typi
  then trace("consuming " ++ show typi ) xs
  else error("Error: Found " ++ (show $ kind x) ++ " -- Expected " 
              ++ show typi ++ " On line " ++ (show $ location x))

head' :: TokenList -> Token
head' (x:_) = x
head' [] = Token "empty" (-1) EOF 
