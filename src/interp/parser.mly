/*              Parser for Lambda-Calculus         */

%{

open Ast
open Exception

%}


%token LAMBDA 
%token EOF
%token LPAR RPAR DOT

%token <Ast.ident> IDENT

%start file

%type <Ast.lambdaF> file

%%

file: 
    | l = application; EOF
		{l}

application:
    | l = atom+ 
    {List.fold_left 
    (fun l1  l2 -> FApply(l1,l2) )
        (List.hd l)
        (List.tl l)
    }

atom :
    | LPAR; a = application; RPAR {a}
    | a = abstraction {a}
    | v = var {v}


ident:
	| i = IDENT 
		{i}

var :
	| i = ident 
		{FVar i}

abstraction :
	| LAMBDA; i = ident; DOT; l = application
		{FLabstract (i,l)}












































