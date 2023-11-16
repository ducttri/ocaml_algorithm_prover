{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | "let" { LET }
 | "(*prove*)" { PROVE }
 | "(*hint: " { hint lexbuf }
 | "(*" { comment 0 lexbuf }
 | ":" { COLON }
 | "(" { LPAREN }
 | ")" { RPAREN }
 | "=" { EQUALS }
 | ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']+ as word { IDENT(word) }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and hint = parse
 | [' ' '\t'] { hint lexbuf }
 | "axiom *)" { AXIOM }
 | "*)" { token lexbuf }
and comment level = parse
 | "*)" { if level = 0 then token lexbuf 
          else comment (level - 1) lexbuf }
 | "(*" { comment (level + 1) lexbuf }
 | _ {comment level lexbuf}
 | eof { raise (SyntaxError "Unclosed comment") }