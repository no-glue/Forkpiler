module Backpatch where
import Data.List
import qualified Data.Map as Map
import SymbolTable

type BackTable = Map.Map (Char,Int) String 

backpatch :: (ScopeMap,BackTable) -> [String] -> Int -> String
backpatch _ [] _ = ""
backpatch (m,t) (x1:x2:xs) counter
  |head x1 == 'I' =
    let 
      name = tail x1
      key = (name,read x2) 
    in
      if Map.member key
        then backpatch (m,t) xs counter ++ (t Map.! key)
        else backpatch (m,t) xs (counter + 
