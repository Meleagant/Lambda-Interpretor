/*              Parser for Lambda-Calculus         */

%{

open Ast

%}


%token LAMBDA 
%token EOF
%token LPAR RPAR DOT

%token <Ast.ident> IDENT

%start file

%type <Ast.lambdaF> file

%%

file: 
	l = lambda; EOF
		{l}

lambda :
	| l = first+ 
		{match l with 
		| [x] -> x
		| t::q -> 
			List.fold_left (fun x y -> FApply (x,y)) t q} 


first:
	| v = var 
		{v}
	| LPAR; l = lambda; RPAR
		{l}
	| l = abstraction
		{l}

ident:
	| i = IDENT 
		{i}

var :
	| i = ident 
		{FVar i}

abstraction :
	| LAMBDA; i = ident; DOT; v = var 
		{FLabstract (i,v)}
	| LAMBDA; i = ident; DOT; a = abstraction 
		{FLabstract (i,a)}
	| LAMBDA; i = ident; DOT; LPAR; l = lambda; RPAR
		{FLabstract (i,l)}












































