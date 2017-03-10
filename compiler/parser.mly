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
%token OR
%token AND
%token NOT
%token EQUAL DIFF
%token PLUS SUB
%token TIMES DIV REM

/* Associativite */

%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL DIFF
%left PLUS MINUS
%left TIMES DIV REM

%start file

%type <MiniML_ast.expr> 

%%

file:
    | e = expr;EOF {e}

expr:
    | i = const {i}
    | i = IDENT {


