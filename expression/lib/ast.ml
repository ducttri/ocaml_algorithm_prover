type expression =
  | Identifier of string
  | Application of expression * expression list 

and pattern =
  | Constructor of string * pattern list
  | Variable of string * string

and equal = 
  | Equality of expression * expression

and hint = 
  | Axiom
  | Induction
  | Direct  

and decl = 
  | Proof of (string * pattern list * equal * hint option)