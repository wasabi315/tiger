# tiger

"Modern Compiler Implementation in ML" in Haskell!

## Syntax

```ebnf
expr
    = "let" , { dec } , "in" , [ expr , { ";" , expr } ] , "end"
    | "if" , expr , "then" , expr , [ "else" , expr ]
    | "while" , expr , "do" , expr
    | "for" , id , ":=" , expr , "to" , expr , "do" , expr
    | "break"
    | "(" , expr , { ";" , expr } , ")"
    | var , ":=" , expr
    | value ;

dec
    = tydec
    | vardec
    | fundec ;

tydec
    = "type" , type-id , "=" , ty ;

ty
    = type-id
    | "{" , tyfields , "}"
    | "array" , "of" , type-id ;

tyfields
    = ε
    | id , ":" , type-id , { "," , id , ":" , type-id } ;

vardec
    = "var" , id , [ ":" , type-id ] , ":=" , expr ;

fundec
    = "function" , id , "(" , tyfields , ")" , [ ":" , type-id ] , "=" , expr ;

var
    = id
    | var , "." , id
    | var , "[" , expr , "]" ;

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
    = "nil"
    | integer
    | string
    | record
    | array
    | var
    | id , "(" , [ expr , { "," , expr } ] , ")"
    | "(" , expr , ")" ;

record
    = type-id , "{" , [ id , "=" , expr , { "," , id , "=" , expr } ] , "}" ;

array
    = type-id , "[" , expr , "]" , "of" , expr ;

id
    = IDENTIFIER ;

type-id
    = IDENTIFIER ;

integer
    = INTEGER ;

string
    = STRING ;

```
