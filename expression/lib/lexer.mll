{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n" 

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | "let" { LET }
 (* | "type" { TYPE } *)
 | "rec" { REC }
 | "with" { WITH }
 | "match" { MATCH }
 | "(*prove*)" { PROVE }
 | "(*hint: axiom *)" { AXIOM }
 | "(*hint: induction" { induction lexbuf}
 | "(*" { comment 0 lexbuf }
 (* | "of" { OF } *)
 | "|" { PATTERN }
 | "*" { PAIR }
 | "->" { ARROW }
 | "," { COMMA }
 | ":" { COLON }
 | "(" { LPAREN }
 | ")" { RPAREN }
 | "=" { EQUALS }
 | ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as word { IDENT(word) }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and induction = parse
 | [' ' '\t'] { induction lexbuf }
 | (['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as var) " *)" { INDUCTION(var) }
and comment level = parse
 | "*)" { if level = 0 then token lexbuf 
          else comment (level - 1) lexbuf }
 | "(*" { comment (level + 1) lexbuf }
 | _ {comment level lexbuf}
 | newline { Lexing.new_line lexbuf; comment level lexbuf }
 | eof { raise (SyntaxError "Unclosed comment") }