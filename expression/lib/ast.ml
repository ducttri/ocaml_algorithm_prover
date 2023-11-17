type expression =
  | Application of (expression * expression list)
  | Identifier of string

type pattern =
  | Constructor of (string * pattern list)
  | Variable of string * string
  (* constructor are like match .... with | a; | b *)
  (* variable are inside when matching for ex Cons(h, append ...) *) 

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
  | Definition of (string * pattern list * string * string * matches list )

  (* Proof of (string * typeVar list * pattern list * equal * hint option) *)