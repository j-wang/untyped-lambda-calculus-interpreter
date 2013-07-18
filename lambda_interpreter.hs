{--| Untyped Lambda Calculus Interpreter

James Wang, 27 Jun 2013

--}

module Interpreter where

data Expression = Lambda String Expression
                | Variable String
                | Application Expression Expression

lexer :: String -> [String]
lexer x:xs = 
    case x of
      '(' -> ["("] ++ lexer xs
      ')' -> [")"] ++ lexer xs
      ' ' -> lexer xs
      _   -> let chunkWord y:ys = 
                     case y of
                       ' ' -> []
                       ')' -> []
                       _   -> [y] ++ chunkWord xs
                 word = chunkWord x:xs
             in
               [word] ++ lexer (length word) x:xs
           
parser :: [String] -> Expression

reducer :: Expression -> Expression

-- main that runs interpreter loop
