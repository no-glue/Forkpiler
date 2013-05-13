module Backpatch where
import Data.List
import Numeric
import qualified Data.Map as Map
import SymbolTable

type BackTable = Map.Map (Char,Int) String 

backpatch :: (ScopeMap,BackTable) -> [String] -> Int -> String
backpatch _ [] _ = ""
backpatch (m,t) (x1:x2:xs) counter
  |head x1 == 'I' =
    let 
      name = head $ tail x1
      scope = findScope m [name] $ read x2
      key = (name,scope)
      ut = tableUpdate t key counter
    in
      if Map.member key t
        then (t Map.! key) ++ " " ++ backpatch (m,t) xs counter
        else (ut Map.! key) ++ " " ++ backpatch (m,ut) xs (counter + 1)
    |otherwise = x1 ++ " " ++ backpatch (m,t) (x2:xs) counter
backpatch _ x _ = unlines x

tableUpdate :: BackTable -> (Char, Int) -> Int -> BackTable
tableUpdate t key value = Map.insert key hexValue t
  where 
    hexValue = byteify $ showHex value ""

byteify :: String -> String
byteify x
  |length x == 1 = "0" ++ x ++ " " ++ "00"
  |length x == 2 = x ++ " " ++ "00"
  |length x == 3 = 
    let (b1:b2:b3:[]) = x
    in (b1:[b2]) ++ " " ++ ('0':[b3])
  |length x == 4 =
    let (b1:b2:b3:b4:[]) = x
    in (b1:[b2]) ++ " " ++ (b3:[b4])

