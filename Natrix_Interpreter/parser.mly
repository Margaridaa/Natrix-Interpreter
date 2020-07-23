
/* Analizador sintáctico para Natrix */

%{
  open Ast
%}

%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token DEF IF THEN ELSE PRINT FOREACH IN AND OR NOT FILLED BY
%token EOF OF LET
%token LP RP LSQ RSQ COMMA EQUAL DO BEGIN END NEWLINE LBRACKET RBRACKET DOTDOT
%token COLON SEMICOLON
%token PLUS MINUS TIMES DIV MOD
%token INT ARRAY TYPE VAR MAXINT MININT

/* Definição das prioridades e associatividades dos tokens */

%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus
%nonassoc LSQ

/* Ponto de entrada da gramática */
%start file

/* Tipo dos valores devolvidos pelo analizador sintáctico */
%type <Ast.file> file

%%

file:
| NEWLINE? dl = list(def) b = nonempty_list(stmt) EOF
    { dl, Sblock b }
;

def:
| DEF f = ident LP x = separated_list(COMMA, ident) RP
  COLON s = suite
    { f, x, s }
;

expr:
| c = CST
    { Ecst c }
| MAXINT
		{ Emax }
| MININT
		{ Emin }
| id = ident
    { Eident id }
| e1 = expr LSQ e2 = expr RSQ
    { Eget (e1, e2) }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| NOT e1 = expr
    { Eunop (Unot, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| f = ident LP e = separated_list(COMMA, expr) RP
    { Ecall (f, e) }
| LSQ e1 = expr DOTDOT e2 = expr RSQ
		{ Elist (e1, e2) }
| LP e = expr RP
    { e }
| e1 = expr DOTDOT e2 = expr
		{ Edotdot (e1, e2) }
| LET id = ident EQUAL e1 = expr IN e2 = expr
    { Eletin (id, e1, e2) }
;

suite:
| l = nonempty_list(stmt2)
    { Sblock l }
| x = simple_stmt
	{ x }
;

simple_stmt:
| id = ident COLON EQUAL e = expr SEMICOLON
    { Sassign (id, e) }
| id = ident LSQ e1 = expr RSQ COLON EQUAL e2 = expr SEMICOLON
    { Sassign2 (id, e1, e2) }
| PRINT LP e = expr RP SEMICOLON
    { Sprint e }
| e = expr
    { Seval e }
;

stmt2:
| s = simple_stmt
		{ s }
| s = stmt 
		{ s } 
;

stmt_block:
| l = nonempty_list(stmt2)
    { Sblock l }
| IF c = expr THEN LBRACKET s1 = stmt_block  RBRACKET ELSE LBRACKET s2 = stmt_block  RBRACKET 
    { Sif (c, s1, s2) }
;

stmt:
| s = simple_stmt NEWLINE
    { s } 
| IF c = expr THEN LBRACKET s1 = stmt_block  RBRACKET ELSE LBRACKET s2 = stmt_block  RBRACKET NEWLINE
    { Sif (c, s1, s2) }
| FOREACH x = ident IN e = expr DO LBRACKET  s = stmt_block  RBRACKET NEWLINE
    { Sforeach (x, e, s) }
| TYPE id = ident EQUAL LSQ e = expr  RSQ SEMICOLON NEWLINE
	{ Stype (id, e) }
| TYPE id = ident EQUAL ARRAY  t = ident OF INT SEMICOLON NEWLINE
	{ Stype2 (id, t) }
| VAR id = ident COLON INT EQUAL e = expr SEMICOLON NEWLINE
	{ Svar (id, e) }
| VAR id = ident COLON a = ident FILLED BY e = expr SEMICOLON NEWLINE
	{ Svar2 (id, a, e) }
| VAR id = ident COLON ARRAY e1 = expr OF INT FILLED BY e2 = expr SEMICOLON NEWLINE
	{ Svar3 (id, e1, e2) }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c =CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

ident:
  id = IDENT { id }
;


