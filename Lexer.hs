import Token
import Text.Regex.Posix
import Data.List

lexer :: String -> [Token]
lexer input lineNumber 
  | input =~ endline :: Bool =
    let token = input =~ endline :: String
      in recurse lineNumber + 1 
  |input =~ whitespace :: Bool = 
    let token = input =~ whitespace :: String
      in recurse lineNumber 
  |input =~ quotation :: Bool =
    let token = input =~ quotation :: String
      in makeToken : recurse lineNumber
  |input =~ parenOpen :: Bool = 
    let token = input =~ parenOpen :: String
      in makeToken : recurse lineNumber
  |input =~ parenClose :: Bool = 
    let token = input =~ parenClose :: String
      in makeToken : recurse lineNumber
  |input =~ equalsOp :: Bool = 
    let token = input =~ equalsOp :: String
      in makeToken : recurse lineNumber
  |input =~ plusOp :: Bool = 
    let token = input =~ plusOp :: String
      in makeToken : recurse lineNumber
  |input =~ minusOp :: Bool =
    let token = input =~ minusOp :: String
      in makeToken : recurse lineNumber
  |input =~ openBrace :: Bool =
    let token = input =~ openBrace :: String
      in makeToken : recurse lineNumber
  |input =~ closeBrace :: Bool =
    let token = input =~ closeBrace :: String
      in makeToken : recurse lineNumber
  where 
    whitespace = "^[ \t\n]"
    endline = "^[\n]"
    quotation = "^[\"]"
    parenOpen = "^[(]"
    parenClose = "^[)]"
    equalsOp = "^[=]"
    plusOp = "^[+]"
    minusOp = "^[-]"
    openBrace = "^[{]"
    closeBrace = "^[}]"
    recurse = lexer (delete (contents token) input)

