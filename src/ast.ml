(*###########################################################################*)
(*                           Lambda-Calculus AST                             *)
(*###########################################################################*)

(*                           First AST                                       *)


type ident = string

type lambdaF = 
	| FVar of ident 
	| FApply of lambdaF*lambdaF
	| FLabstract of ident*lambdaF


(*                   Work AST           *)

type lambdaW = 
	| WVar of int
	| WApply of lambdaW*lambdaW
	| WLTerme of lambdaW


(* type for \-calculus *)

type ty = 
	| Basic of int
	| App of ty*ty



























































