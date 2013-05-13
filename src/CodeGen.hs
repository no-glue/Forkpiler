module CodeGen where
import AST
import SymbolTable
import Token
import Debug.Trace

--type TempTable = Map.Map Int (String
ldaI = "A9 "
ldaM = "AD "
sta  = "8D "
adc  = "6D "
ldxI = "A2 "
ldxM = "AE "
ldyI = "A0 "
ldyM = "AC "
nop  = "EA "
brk  = "00 "
cpx  = "EC "
bne  = "D0 "
inc  = "EE "
sys  = "FF "
int c = "0" ++ c ++ " "
placeHolder name scope = "T" ++ name ++ " " ++ (show scope) ++ " "

codeGen :: ScopeMap -> AST -> Int -> String
codeGen m (AST parent []) s = ""
codeGen m (AST parent (child:children)) s
  -- |tt == PlusOp = math m (child:children)
  |tt == PrintOp = printGen m child
  |tt == EqualsOp = assignGen m (child:children) s
  |tt == OpenBrace =
    let nextScope = scope (tokentype parent) 
    in foldr (\c s -> (codeGen m c nextScope) ++ s) "" (child:children)
  |otherwise = codeGen m child s 
  where
    tt = kind $ original parent

assignGen :: ScopeMap -> [AST] -> Int -> String
assignGen _ [] _ = ""
assignGen m (left:right:[]) scope
  |rk == Digit = ldaI ++ int (contents rt) ++ sta ++ placeHolder (contents lt) scope 
  |rk == PlusOp = 
    let sum = math m rightKids
    in sum ++ ldaM ++ "00 00 " ++ sta ++ placeHolder (contents lt) scope
  |otherwise = error "uhoh"
  where
    (AST rightParent rightKids) = right
    (AST leftParent leftKids) = left
    rt = original rightParent
    rk = kind rt
    lt = original leftParent

printGen :: ScopeMap -> AST -> Int -> String
--printGen m [] = error "you ain't trying to print nothign!"
printGen m child scope
  |tt == Digit = ldxI ++ "01 " ++ ldyI ++ int (contents token) ++ sys
  |tt == PlusOp = (math m children) ++ ldxI ++ "01 " ++ ldyM ++ "00 00 " ++ sys
  |otherwise = error "this ain't done yet!"
  where
    (AST parent children) = child
    token = original parent
    tt = kind token

math :: ScopeMap -> [AST] -> String
math m [] = error "plus opperator has no children"
math m (left:right:[])
  |rk == PlusOp = leftString ++ math m rightKids 
  |otherwise = leftString ++ rightString 
  where
    (AST rightParent rightKids) = right
    (AST leftParent leftKids) = left
    rt = original rightParent
    rk = kind rt
    lt = original leftParent
    leftString = ldaI ++ int (contents lt) ++ sta ++ "00 00 " 
    rightString = ldaI ++ int (contents rt) ++ adc ++ "00 00 " ++ sta ++ "00 00 "
