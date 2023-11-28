include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_declaration (d : declaration) : string
 = match d with
 | Proof (name, variables, equal, hint) -> "let (*prove*) " ^ name ^ "" ^ string_of_pattern_list variables ^ " = (" ^ (string_of_equal equal) ^ ") \n" ^ (string_of_hint hint)
 | Type (name, pattern) -> "type " ^ name ^ " = " ^ string_of_type_list pattern
 | Definition (name, variables, output, varmatch, matchlist) -> "let rec " ^ name ^ " " ^ string_of_pattern_list variables ^ ": " ^ output ^ 
 " = match " ^ varmatch ^ " with " ^ string_of_matches_list matchlist


 and string_of_expression (e : expression)
 =  match e with
 | Identifier nm -> nm
 | Application (e1, []) -> " " ^ (string_of_expression e1)
 | Application (e1, expressions) -> (string_of_expression e1) ^ " (" ^ (String.concat ", " (List.map string_of_expression expressions)) ^ ")"

and string_of_equal (e : equal) : string
= match e with
 | Equality (e1, e2) -> string_of_expression e1 ^ " = " ^ string_of_expression e2 

and parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast

(* parse expression tester *)
(* instead of writing a whole proof we can use paste an expression to test *)
and parse_expression (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof Lexer.token lexbuf in ast 
(* uncomment this might be better *)

and string_of_hint (h : hint option) : string =
  match h with
  | None -> " "
  | Some Axiom -> "(*hint: axiom *)"
  | Some Induction (var) -> "(*hint: induction " ^ var ^ " *)"

and string_of_pattern_list (pl : pattern list) : string =
  List.fold_left (fun i x -> i ^ string_of_pattern x ^ " ") " " pl 

and string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> "| " ^ name 
  | Constructor (name, patterns) -> "| " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  | Variable (nm1, nm2) -> "(" ^ nm1 ^ " : " ^ nm2 ^ ")" 

and string_of_matches_list (ml : matches list) : string =
  List.fold_left (fun i x -> i ^ string_of_matches x ^ "\n") " " ml 

and string_of_matches (m : matches) : string =
  match m with
  | Case (pattern, expression) -> string_of_pattern pattern ^ " -> " ^ string_of_expression expression

and string_of_type_list (pl : pattern list) : string =
  List.fold_left (fun i x -> i ^ string_of_type x ^ " ") " " pl 

and string_of_type (p : pattern) : string =
  match p with
  | Constructor (name, []) -> "| " ^ name 
  | Constructor (name, patterns) -> "| " ^ name ^ " of (" ^ (String.concat " * " (List.map string_of_type patterns)) ^ ")"
  | Variable (nm1, _) -> nm1
    


(*MATCHING*)
(* singleton, empty, find, merge (funciton), subtitute (function) *)
(* print_subt can be delete *)
(* let Some result = match_expression ... *)
(* Subtution.print_subt result *)

(* for merge, we can try to use iter + mem to check whether they are in the Map *)
(* double check lecture 29 for more information on case testing *)


module Substitution = struct
  module MM = Map.Make(String)
  type t = expression MM.t  
  let empty = MM.empty
  let singleton = MM.singleton
  let find = MM.find
  let mem = MM.mem
  let merge mp1 mp2 = 
    match (mp1, mp2) with
    | (None, _) -> None
    | (_, None) -> None
    | (Some map1, Some map2) -> Some (MM.merge (fun k l r -> 
      if (MM.find_opt k map1) == None then (if (MM.find_opt k map2) == None then None else r) else (if (MM.find_opt k map2) == None then l else (if find k map1 = find k map2 then l else None ))) map1 map2)
  let print_subt (s : t option) =
    match s with
    | None -> print_endline ("end")
    | Some str -> MM.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_expression v)) str
end

(* match case watchout for unequal !!! *)

(* let rec match_expression (var : string list) (pattern : expression) (goal : expression) : Substitution.t option =
 match pattern with
 | Identifier nm -> if List.mem nm var then (Some (Substitution.singleton nm goal)) else 
                    (if goal = Identifier nm then Some Substitution.empty else Some Substitution.empty)
 | Application (_, []) -> None
 | Application (e1, etl) -> match goal with
          | Identifier _ -> None
          | Application (_, []) -> None
          | Application (e2, etl2) -> (Substitution.merge (match_expression var e1 e2) (match_expression_list var etl etl2)) *)
            
let rec match_expression (var : string list) (pattern : expression) (goal : expression) : Substitution.t option =
  match pattern with
  | Identifier nm -> (match goal with 
                    | Identifier nm2 -> if ((List.mem nm var) || (nm = nm2)) then 
                      (if List.mem nm var then (Some (Substitution.singleton nm goal)) else (Some Substitution.empty)) else None
                    | Application (_,_) -> if List.mem nm var then (Some (Substitution.singleton nm goal)) else None)
  | Application (_, []) -> None
  | Application (e1, etl) -> match goal with
            | Identifier _ -> None
            | Application (_, []) -> None
            | Application (e2, etl2) -> (Substitution.merge (match_expression var e1 e2) (match_expression_list var etl etl2))

and match_expression_list (var : string list) (pattern : expression list) (goal : expression list) : Substitution.t option =
  match pattern with
  | [] -> Some Substitution.empty
  | (h::tl) -> match goal with
            | (h2::tl2) -> (Substitution.merge (match_expression var h h2) (match_expression_list var tl tl2))
            | [] -> Some Substitution.empty



let rec attemptRewrite (var : string list) (eq : equal) (expr : expression) : expression option =
  match eq with
  | Equality (lhs, rhs) -> match match_expression var lhs expr with
              | Some map -> if List.for_all (fun k -> Substitution.mem k map) var then Some (rewriterMap var rhs map) else None
              | None -> (match expr with
                        | Application(Identifier x, b::tl) -> (match attemptRewrite var eq b with
                                                              | Some v -> Some (Application (Identifier x, v::tl))
                                                              | None -> None)
                        | Application(Application(a,b), h::tl) -> (match attemptRewrite var eq (Application(a,b)) with
                                                              | Some v -> Some (Application (v, h::tl))
                                                              | None -> match attemptRewrite var eq h with
                                                                       | Some v -> Some (Application(Application(a,b), v::tl))
                                                                       | None -> None)
                        | Application(_, []) -> None
                        | Identifier _ -> None)

and rewriterMap (var : string list) (expr : expression) (map : Substitution.t) : expression =
  match expr with
  | Identifier nm -> if Substitution.mem nm map then Substitution.find nm map else Identifier nm
  | Application (e, []) -> rewriterMap var e map
  | Application (e, expressions) -> Application (rewriterMap var e map, List.map (fun ex1 -> rewriterMap var ex1 map) expressions)

let rec tryEqualities (expr : expression) (eql : (string * string list * equal) list) : (string * expression) option =
  match eql with
  | [] -> None
  | ((name, var, eq)::tl) -> match attemptRewrite var eq expr with
                             | None -> tryEqualities expr tl 
                             | Some expression -> Some (name, expression)

let rec performSteps (expr : expression) (eql : (string * string list * equal) list) : (string * expression) list =
  match tryEqualities expr eql with
  | None -> []
  | Some (nm, expre) -> (nm, expre) :: performSteps expre eql 

let string_of_expression_2 (e : expression) : string =
  "  " ^ (string_of_expression e)

let rec stepToString (lst : (string * expression) list) : string list =
  match lst with
  | [] -> []
  | ((nm, expr) :: tl) -> (" = { " ^ nm ^ " }") :: (string_of_expression_2 expr) :: (stepToString tl)

let rec getLast (lst : string list) : string option =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _::tl -> getLast tl

let produceProof (eq : equal) (eql : (string * string list * equal) list) : string list =
  match eq with
  | Equality(lhs,rhs) -> if (getLast ([string_of_expression_2 lhs] @ stepToString (performSteps lhs eql))) = (getLast ([string_of_expression_2 rhs] @ stepToString(performSteps rhs eql)))
                          then (match List.rev ([string_of_expression_2 rhs] @ stepToString(performSteps rhs eql)) with 
                                | _::tl -> [string_of_expression_2 lhs] @ stepToString (performSteps lhs eql) @ tl
                                | [] -> [string_of_expression_2 lhs] @ stepToString (performSteps lhs eql))
                          else [string_of_expression_2 lhs] @ stepToString (performSteps lhs eql) @ [" = ??? Could not determine a next proof step ???"] @ stepToString(performSteps rhs eql) @ [string_of_expression_2 rhs]

let rec patternToString (pl : pattern list) : string list =
  match pl with
  | ((Variable (nm1, _))::tl) -> nm1 :: patternToString tl
  | _ -> []

let rec proofs_of_simple eqs (lst : declaration list) =
  match lst with
  | [] -> []
  | (Proof (name, variables, equal, hint))::decls -> 
      (match hint with
      | None -> (("Proof of "^name^": ") :: (produceProof equal eqs)) :: (proofs_of_simple ((name, (patternToString variables), equal) :: eqs) decls)
      | _ -> proofs_of_simple ((name, (patternToString variables), equal) :: eqs) decls)
  | _::decls -> proofs_of_simple eqs decls 

let produce_output_simple (lst : declaration list)  =
  print_endline (String.concat "\n\n"
(List.map (String.concat "\n")
(proofs_of_simple [] lst)))