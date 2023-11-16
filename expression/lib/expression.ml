include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_declaration (d : declaration) : string
 = match d with
 | Proof (name, variable, equal, hint) -> "let (*prove*) " ^ name ^ " " ^ string_of_variable_list variable ^ " = (" ^ (string_of_equal equal) ^ ") \n" ^ (string_of_hint hint)

 and string_of_expression (e : expression)
 =  match e with
 | Identifier nm -> nm
 | Application (e1, []) -> " " ^ (string_of_expression e1)
 | Application (e1, expressions) -> (string_of_expression e1) ^ " (" ^ (String.concat ", " (List.map string_of_expression expressions)) ^ ")"

(* and string_of_expression (e : expression)
 =  match e with
 | Identifier nm -> nm
 | Application (e1, e2) ->
  (string_of_expression e1) ^ 
  " " ^ (string_of_expression_with_parens e2) *)

(* and string_of_expression_with_parens (e : expression) : string
 = match e with 
 | Identifier nm -> nm
 | Application _ -> "(" ^ string_of_expression e ^ ")" *)

and string_of_equal (e : equal) : string
= match e with
 | Equality (e1, e2) -> string_of_expression e1 ^ " = " ^ string_of_expression e2 

and parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast

and string_of_variable_list (vl : typeVar list) : string =
  List.fold_left (fun i x -> i ^ string_of_variable x ^ " ") "" vl 

and string_of_variable (var : typeVar) : string =
  match var with
  | Var (name, vartype) -> " (" ^ name ^ " : " ^ vartype ^ ")"

and string_of_hint (h : hint option) : string =
  match h with
  | None -> " "
  | Some Axiom -> "(*hint: axiom *)"
  | Some Induction (var) -> "(*hint: induction " ^ var ^ " *)"

and string_of_pattern_list (pl : pattern list) : string =
  List.fold_left (fun i x -> i ^ string_of_pattern x ^ "\n|") "|" pl 

and string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  | Variable (nm1, nm2) -> nm1 ^ ": " ^ nm2