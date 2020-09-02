# tiger

"Modern Compiler Implementation in ML" in Haskell!

## Syntax

```ebnf
exp
    = "let" , { dec } , "in" , [ exp , { " ;" , exp } ] , "end"
    | "if" , exp , "then" , exp , [ "else" , exp ]
    | "while" , exp , "do" , exp
    | "for" , id , ":=" , exp , "to" , exp , "do" , exp
    | "break"
    | "(" , exp , { " ;" , exp } , ")"
    | lvalue , ":=" , exp
    | value ;

dec
    = tydec
    | vardec
    | fundec ;

tydec
    = "type" , type-id , "=" , ty ;

ty
    = type-id
    | "{" , [ tyfields ] , "}"
    | "array" , "of" , type-id ;

tyfields
    = id , ":" , type-id , { "," , id , ":" , type-id } ;

vardec
    = "var" , id , [ ":" , type-id ] , ":=" , exp ;

fundec
    = "function" , id , "(" , tyfields , ")" , [ ":" , type-id ] , "=" , exp ;

lvalue
    = id
    | lvalue , "." , id
    | lvalue , "[" , exp , "]" ;

value
    = disjunction ;

disjunction
    = conjunction , { "|" , conjunction } ;

conjunction
    = comparison , { "&" , comparison } ;

comparison
    = additive , [ ( "=" | "!=" | "<=" | "<" | ">=" | ">" ) , additive ] ;

additive
    = multiplicative , { ( "+" | "-" ) , multiplicative } ;

multiplicative
    = unary , { ( "*" | "/" ) , unary } ;

unary
    = [ "-" ] , primary ;

primary
    = "(" , ")"
    | "nil"
    | integer
    | string
    | lvalue
    | id , "(" , [ exp , { "," , exp } ] , ")"
    | "(" , exp , ")" ;

id
    = IDENTIFIER ;

type-id
    = IDENTIFIER ;

integer
    = INTEGER ;

string
    = STRING ;

```
