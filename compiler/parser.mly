/*       Analyseur syntaxique pour Mini-ML          */

%{

open Ada_ast
open Exception

%}

/* Declaration des tokens */

%token EOF
%token <MiniML_ast.ident> IDENT
%token <MiniML_ast.cons> CONST

%token LPAR RPAR
%token IF THEN ELSE
%token OR
%token AND
%token NOT
%token EQUAL DIFF
%token PLUS SUB
%token TIMES DIV REM
%token COMMA
%token TRUE FALSE
%token LET
%token IN
%token FUN REC ARROW

/* Associativite */

%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL DIFF
%left PLUS
%left TIMES DIV REM

%start file

%type <MiniML_ast.expr> file 

%%

file:
    | e = expr;EOF {e}

expr:
	| LPAR; e = expr; RPAR {e}
    | i = const {i}
    | i = ident {i}
	| i = ident; arg = separated_nonempty_list(COMMA,expr)
		{Call (id,arg)}
	| e1 = expr; b = binop; e2 = expr
		{Binop (b,e1,e2)}
	| u = unop; e = expr 
		{Unop (u,e)}
	| IF; e = expr; THEN; e1  = expr; ELSE; e2 = expr
		{If (e,e1,e2)}
	| LET; i = IDENT; FUN; arg =  separated_nonempty_list(COMMA, ident);
	ARROW; e1 = expr; IN; e = expr
		{LetFun (id, arg, e1,e)}
	| LET; REC; i = ident; FUN; arg =
	separated_nonempty_list(COMMA,ident);
	ARROW; eret = expr ; IN; e = expr
		{LetFunRec (id,arg,eret,e)}
	| LET; i = ident; EQUAL; eval = expr; IN; e = expr
		{LetVar (id,eval,e)}

ident:
	| i = IDENT {i}

const:
	| a = CONST {a}
	| TRUE {Const (Bool (true))}
	| FALSE {Const (Bool (false))}

unop:
    | NOT {NOT}

binop:
    | PLUS {ADD}
    | SUB {SUB}
    | TIMES {MULT}
    | DIV {DIV}
    | REM {REM}
    | AND {AND}
    | OR {OR}
    | EQUAL {EQUAL}
    | DIFF {DIFF}
