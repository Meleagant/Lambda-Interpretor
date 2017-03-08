(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*            Pretty printer for lambda calculus           *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

open Ast

let lbda = String.make 1 '\x5c'

let rec print lambda = 
match lambda with
| FVar id  -> id
| FApply (l1,l2) -> print l1 ^" "^ (print l2)
| FLabstract (id,l) ->
	match l with
	| FApply _ -> lbda ^id ^".("^(print l)^")"
	| _ -> lbda ^id^"."^(print l)

let p l = Printf.printf "%s \n" (print l)

let rec print_par = function
| FVar id -> id
| FApply (l1,l2) ->
	"("^(print_par l1)^" "^(print_par l2)^")"
| FLabstract (id,l) ->
	"("^lbda^id^"."^(print_par l)^")"

let p_par l = Printf.printf "%s \n" (print_par l)
