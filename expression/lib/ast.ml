type expression =
  | Application of expression * expression 
  (* Application of expression * expression list *)
  | Identifier of string

type pattern =
  | Constructor of (string * pattern list)
  | Variable of (string * string)
  (* constructor are like match .... with | a; | b *)
  (* variable are inside when matching for ex Cons(h, append ...) *)
type typeVar =
  | Var of (string * string) 
  (* var name and type *)

type equal = 
  | Equality of expression * expression

type hint = 
  | Axiom 
  (* | Induction of string *)

type declaration = 
  | Proof of (string * typeVar * equal * hint)

  (* Proof of (string * typeVar list * pattern list * equal * hint option) *)