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

(* and parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast *)

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
  let merge mp1 mp2 = 
    match (mp1, mp2) with
    | (None, _) -> None
    | (_, None) -> None
    | (Some map1, Some map2) -> Some (MM.merge (fun k l r -> 
      if (MM.find_opt k map1) == None then r else (if (MM.find_opt k map2) == None then l else None)) map1 map2)
  let print_subt (s : t option) =
    match s with
    | None -> print_endline ("end")
    | Some str -> MM.iter (fun k v -> print_endline (k ^ " -> " ^ string_of_expression v)) str
end

(* match case watchout for unequal !!! *)

let rec match_expression (var : string list) (pattern : expression) (goal : expression) : Substitution.t option =
 match pattern with
 | Identifier nm -> if List.mem nm var then (Some (Substitution.singleton nm goal)) else 
                    (if goal = Identifier nm then Some Substitution.empty else None)
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

(* let attemptRewrite (var : string list) (eq : equal) (expr : expression) : expression option =
  match eq with
  | Equality (lhs, rhs) -> match match_expression var lhs expr with
                           | 
                           | None ->  *)