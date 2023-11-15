include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_declaration (d : declaration)
 = match d with
 | Proof (name, variable, equal, hint) -> "let (*prove*) " ^ name ^ " " ^ string_of_variable variable ^ " = (" ^ (string_of_equal equal) ^ ") \n (*hint: " ^ (string_of_hint hint) ^ "*)"  

and string_of_expression (e : expression)
 =  match e with
 | Identifier nm -> nm
 | Application (e1, e2) ->
  (string_of_expression e1) ^ 
  " " ^ (string_of_expression_with_parens e2)

and string_of_expression_with_parens e 
 = match e with 
 | Identifier nm -> nm
 | Application _ -> "(" ^ string_of_expression e ^ ")"

and string_of_equal (e : equal) : string
= match e with
 | Equality (e1, e2) -> string_of_expression e1 ^ " = " ^ string_of_expression e2

and parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  ast

and string_of_variable (var : typeVar) : string =
  match var with
  | Var (name, vartype) -> " (" ^ name ^ " : " ^ vartype ^ ")"

and string_of_hint (h : hint) : string =
  match h with
  | Axiom -> "Axiom"