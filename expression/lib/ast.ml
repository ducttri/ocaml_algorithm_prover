type expression =
  | Application of expression * expression 
  | Identifier of string


type pattern =
  | Constructor of string * pattern list
  | Variable of string * string

type equal = 
  | Equality of expression * expression

type hint = 
  | Axiom 

type declaration = 
  | Proof of (string * pattern * equal * hint)

  (* Proof of (string * pattern list * equal * hint option) *)