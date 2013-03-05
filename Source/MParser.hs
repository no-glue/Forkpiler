module MParser where

import Token
import SymbolTable

--An attempt to rewrite my parser using monads

parse tokens = return program tokens

program tokens = do
 return statement tokens 
  
statement (token:next:rest) = do
  case token of
    PrintOp -> do
      consumeToken ParenOpen next:[]
      remaining <- exper rest
      return consumeToken ParenClose remaining
    _ -> error("what you trying to pull")

      
exper (token:next:rest) = do 
  case token of 
    CharacterList ->
      return next:rest
    _ ->
      error("not implemented")

consumeToken typi (x:xs)
  |kind x == typi = xs
  |otherwise = error("didn't find it") 
