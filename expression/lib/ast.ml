type expression =
  | Application of (expression * expression list)
  | Identifier of string

type pattern =
  | Constructor of (string * pattern list)
  | Variable of string * string
  
type equal = 
  | Equality of expression * expression

type hint =
  | Axiom 
  | Induction of string

type matches = 
  | Case of (pattern * expression)

type declaration = 
  | Proof of (string * pattern list * equal * hint option)
  | Type of (string * pattern list)
  | Definition of (string * pattern list * string * string * matches list)
  