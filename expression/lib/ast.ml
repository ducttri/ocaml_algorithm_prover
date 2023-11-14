type expression =
  | Identifier of string
  | Application of expression * expression

and var =
  | Variable of (string * string) list

and equal = 
  | Equality of expression * expression

and hint = 
  | Axiom
  | Induction
  | Direct  

and decl = 
  | Proof of (string * var list * equal * hint option)