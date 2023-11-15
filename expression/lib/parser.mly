%{
    open Ast
%}
%token <string> IDENT
%token LPAREN
%token RPAREN
%token EQUALS
%token COLON
%token LET
%token EOF
%token <string> HINT
%start main
%type <declaration list> main
%type <declaration> declaration
%type <expression> expression
%type <pattern> pattern
%type <equal> equal
%type <hint> hint
%%
main:
| d = declaration ; EOF { [d] }

declaration:
| LET ; nm = IDENT; pt = pattern; EQUALS; eq1 = equal; hint = hint
  { Proof (nm, pt, eq1, hint) }

hint:
| { Axiom }

pattern:
| LPAREN ; nm1 = IDENT ; COLON ; nm2 = IDENT ; RPAREN  { Variable (nm1, nm2) }
| nm1 = IDENT ; p1 = list(pattern) { Constructor (nm1, p1) }

expression:
| LPAREN ; e = expression ; RPAREN { e }
| nm = IDENT { Identifier nm }
| e1 = expression; nm = IDENT 
  { Application (e1, Identifier nm) }
| e1 = expression; LPAREN; e2 = expression; RPAREN
  { Application (e1, e2) }

equal:
| LPAREN ; e1 = expression; EQUALS ; e2 = expression ; RPAREN 
  { Equality (e1, e2) }