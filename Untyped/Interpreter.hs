{--| Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

--}

module Untyped.Interpreter where

data Expression = Variable String
                | Lambda Expression Expression
                | Application Expression Expression
                | Group Expression

lexer :: String -> [String]
lexer [] = []
lexer (x:xs) = 
    case x of
      '(' -> "(" : lexer xs
      ')' -> ")" : lexer xs
      '.' -> "." : lexer xs
      ' ' -> lexer xs
      _   -> let chunkWord [] = []
                 chunkWord (y:ys) = 
                     case y of
                       ' ' -> []
                       ')' -> []
                       '.' -> []
                       _   -> y : chunkWord ys
                 word = chunkWord (x:xs)
             in word : (lexer $ drop (length word) (x:xs))

{-- In progress
firstParse :: [String] -> [Expression]
firstParse [] = []
firstParse (x:xs) =
    case x of
      "(" -> let chunkGroup [] = []
                 chunkGroup (y:ys) =
                     case y of
                       ")" -> []
                       _   -> y : chunkGroup ys
                 group = chunkGroup (xs)
             in firstParse group : firstParse $ drop (length group) (x:xs)
      "lambda" -> let chunkLambda [] = []
                      chunkLambda (y:ys) =
                          case y of
                            ")" -> []
                            
parser :: [String] -> Expression
parser [] = ()
parser (x:xs) =
    case x of
      "(" -> let chunkGroup [] = []
                 chunkGroup (y:ys) = 
                     case y of
                       ")" -> []
                       _   -> y : chunkGroup ys
                 group = chunkGroup (xs)
             in Group (parser group) : (parser $ drop (length group + 1) (xs))
      "lambda" -> let chunkLambda [] = []
                      chunkLambda (y:ys) = 
                          case y of
                            "." -> 

--}

-- subparser that creates a list of expressions, then gets applied in scope?
                       

-- reducer :: Expression -> Expression

-- main that runs interpreter loop
