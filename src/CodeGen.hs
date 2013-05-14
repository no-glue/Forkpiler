module CodeGen where
import AST
import Numeric
import SymbolTable
import Token
import Debug.Trace
import Backpatch

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
jumpTarget c = if length c == 2 then c else "0" ++ c ++ " "
placeHolderInt name scope = "I" ++ name ++ " " ++ (show scope) ++ " "

codeGen :: ScopeMap -> AST -> Int -> String
codeGen m (AST parent []) s = ""
codeGen m (AST parent (child:children)) s
  -- |tt == PlusOp = math m (child:children)
  |tt == PrintOp = printGen m child s
  |tt == EqualsOp = assignGen m (child:children) s
  |tt == OpenBrace =
    let nextScope = scope (tokentype parent) 
    in foldr (\c s -> (codeGen m c nextScope) ++ s) "" (child:children)
  |tt == If = ifGen m (child:children) s 0
  |tt == While = whileGen m (child:children) s
  |otherwise = codeGen m child s 
  where
    tt = kind $ original parent

assignGen :: ScopeMap -> [AST] -> Int -> String
assignGen _ [] _ = ""
assignGen m (left:right:[]) scope
  |rk == Digit = intBool (contents rt) (contents lt)
  |rk == PlusOp = 
    let sum = math m rightKids scope
    in sum ++ ldaM ++ "00 00 " ++ sta ++ placeHolderInt (contents lt) scope
  |rk == TrueOp = intBool "1" $ contents lt
  |rk == FalseOp = intBool "0" $ contents lt
  |otherwise = error "uhoh"
  where
    (AST rightParent rightKids) = right
    (AST leftParent leftKids) = left
    rt = original rightParent
    rk = kind rt
    lt = original leftParent
    intBool value name = ldaI ++ int value ++ sta ++ placeHolderInt name scope

printGen :: ScopeMap -> AST -> Int -> String
--printGen m [] = error "you ain't trying to print nothign!"
printGen m child scope
  |tt == Digit = setPrint ++ ldyI ++ int (contents token) ++ sys
  |tt == PlusOp = (math m children scope) ++ setPrint ++ ldyM ++ "00 00 " ++ sys
  |tt == ID = setPrint ++ ldyM ++ placeHolderInt (contents token) scope ++ sys
  |tt == TrueOp = setPrint ++ ldyI ++ int "1" ++ sys
  |tt == FalseOp = setPrint ++ ldyI ++ int "0" ++ sys
  |otherwise = error "this ain't done yet!"
  where
    setPrint = ldxI ++ "01 "
    (AST parent children) = child
    token = original parent
    tt = kind token

math :: ScopeMap -> [AST] -> Int -> String
math _ [] _ = ""
math m (left:right:[]) scope
  |rk == PlusOp =  math m rightKids scope ++ leftAfterRight
  |rk == ID = leftString ++ sumInt
  |otherwise = leftString ++ rightString 
  where
    (AST rightParent rightKids) = right
    (AST leftParent leftKids) = left
    rt = original rightParent
    rk = kind rt
    lt = original leftParent
    leftString = ldaI ++ int (contents lt) ++ sta ++ "00 00 " 
    leftAfterRight = ldaI ++ int (contents lt) ++ adc ++ "01 00 " 
      ++ sta ++ "00 00 " ++ sta ++ "01 00 "
    rightString = ldaI ++ int (contents rt) ++ adc ++ "00 00 " 
      ++ sta ++ "01 00 " ++ sta ++ "00 00 "
    sumInt = ldaM ++ "00 00 " ++ adc ++ placeHolderInt (contents rt) scope
      ++ sta ++ "00 00 "

--assumes that the left side of the equality is placed in the X register
--the right side is placed in 00 00
equalityGen :: ScopeMap -> [AST] -> Int -> String
equalityGen _ [] _ = ""
equalityGen m (left:right:[]) scope = genLeft ++ " " ++ genRight
  where
    (AST rightParent rightKids) = right
    (AST leftParent leftKids) = left
    rt = original rightParent
    lt = original leftParent
    genLeft
      |kind lt == TrueOp = ldxI ++ int "1"
      |kind lt == FalseOp = ldxI ++ int "0"
      |kind lt == Digit = ldxI ++ int (contents lt)
      |kind lt == PlusOp = math m leftKids scope ++ ldxM ++ "00 00 "
      |kind lt == ID = ldxM ++ placeHolderInt (contents lt) scope
    genRight
      |kind rt == TrueOp = ldaI ++ int "1" ++ sta ++ "00 00 "
      |kind rt == FalseOp = ldaI ++ int "0" ++ sta ++ "00 00 "
      |kind rt == Digit = ldaI ++ int (contents rt) ++ sta ++ "00 00 "
      |kind rt == PlusOp = math m leftKids scope
      |kind rt == ID = ldaM ++ placeHolderInt (contents rt) scope ++ sta ++ "00 00 "
      
ifGen :: ScopeMap -> [AST] -> Int -> Int -> String
ifGen m (equals:block:[]) scope offset
  |kind lt == TrueOp = codeGen m block scope
  |kind lt == FalseOp = ""
  |kind lt == Equality =
     let
      equal = equalityGen m leftKids scope
      blockParty = codeGen m block scope
      jumpDistance = jumpTarget $! showHex ((length $ words blockParty) + offset) ""
     in equal ++ cpx ++ "00 00 " ++ bne ++ jumpDistance ++ " " ++ blockParty
  |otherwise = error "this shit aint done"
  where
    (AST rightParent rightKids) = block
    (AST leftParent leftKids) = equals
    rt = original rightParent
    lt = original leftParent

whileGen ::ScopeMap -> [AST] -> Int -> String
whileGen m asts scope =
  let
    top = ifGen m asts scope 12
    jumpDistance = jumpTarget $ showHex (244 - (length $! words top)) ""
  in top ++ jmp ++ trace("DISTANCE" ++ (show $ length $ (words top))) jumpDistance ++ " "
  where 
    jmp = ldaI ++ "00 " ++ sta ++ "00 00 " ++ ldxI ++ "01 " ++
     cpx ++ "00 00 " ++ bne
