# Untyped Lambda Calculus Interpreter
Interpreter for the untyped lambda calculus described in [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/).

##Capabilities
Only the Python version is completed and working. The Haskell version still in progress.

The interpreter can parse and evaluate basic abstractions made up of lambda statements, variables, and function applications. There is no environment currently implemented, so variables cannot represent expressions other than themselves.

To do:

- Dynamic environment allowing symbolic look up and evaluation
- Closures (which requires the previous)

##Examples
    LCI (q to quit) >> (lambda x. x)
    lambda x. x
    LCI (q to quit) >> (lambda x. x y) t
    t y
    LCI (q to quit) >> (lambda x. x) ((lambda x. x) (lambda z. (lambda x. x) x))
    lambda z. (lambda x. x) x
