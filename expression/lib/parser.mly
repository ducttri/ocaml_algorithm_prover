%{
    open Ast
%}
%token <string> IDENT
%token LPAREN
%token RPAREN
%token EQUALS
%token COLON
%token LET
// %token TYPE
%token EOF
%token PROVE
%token AXIOM
// %token PATTERN
// %token MATCH
%start main
%type <declaration list> main
%type <declaration> declaration
%type <expression> expression
// %type <pattern> pattern
%type <equal> equal
%type <hint> hint
%type <typeVar> typeVar
%%
main:
| d = declaration ; EOF { [d] }

declaration:
| LET ; PROVE ; nm = IDENT; var = list(typeVar); EQUALS; eq1 = equal; hint = hint
  { Proof (nm, var, eq1, hint) }
// | LET ; TYPE ; pt = list(pattern)

hint:
| AXIOM { Axiom }

typeVar:
| LPAREN ; nm1 = IDENT ; COLON ; nm2 = IDENT ; RPAREN  { Var (nm1, nm2) }

// pattern:
// | LPAREN ; nm1 = IDENT ; COLON ; nm2 = IDENT ; RPAREN  { Variable (nm1, nm2) }
// | PATTERN; nm1 = IDENT ; 

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