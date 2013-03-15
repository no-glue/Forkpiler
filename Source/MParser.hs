module MParser where

import Token
import SymbolTable

--An attempt to rewrite my parser using monads

parse tokens = return (program tokens)

program tokens = do
 return (statement tokens)
  
statement :: Monad m => TokenList -> m TokenList
statement (token:next:rest) = do
  case (kind token) of
    PrintOp -> do
      consumeToken ParenOpen next
      remaining <- exper rest
      consumeToken ParenClose (head' remaining)
      return (tail remaining)
    _ -> error("what you trying to pull")

exper :: Monad m => TokenList -> m TokenList
exper (token:next:rest) = do
  case (kind token) of
    CharacterList ->
      return (next:rest)
    _ ->
      error("not implemented")

consumeToken typi x
  |kind x /= typi = error("didn't find it")

head' :: TokenList -> Token
head' (x:xs) = x
head' [] = Token "empty" (-1) EOF 
