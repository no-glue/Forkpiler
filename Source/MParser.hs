module MParser where

import Token
import SymbolTable

--An attempt to rewrite my parser using monads

parse :: TokenList -> (SymbolTable, [String])
parse tokens = return program tokens

program tokens = do
 return statement (tokens, [], [])
  
statement ([],table,errors) = do
  return ([], table, ("Error: Recived empty token list in statement. " ++
    "Expected: Print, ID, open brace, int or char; Got: Nothing"):errors);
statement ((token:next:rest),table,errors) = do
  case token of
    PrintOp -> do
      consumeToken ParenOpen next:[]
      remaining <- exper rest
      return consumeToken ParenClose remaining
    _ ->
      error("what you trying to pull")

exper ((token:rest),table,errors) = do
  case token of 
    CharacterList ->
      return rest
    _ ->
      error("not implemented")

consumeToken :: TokenType -> TokenList -> TokenList
consumeToken type' (x:xs)
  |kind x == type' = xs
  |otherwise = error("didn't find it") 
