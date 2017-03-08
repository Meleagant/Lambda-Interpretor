(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*            Pretty printer for lambda calculus           *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

open Ast

let lbda = String.make 1 '\x5c'

let par str = "("^str^")"

let rec print lambda = 
match lambda with
| FVar id  -> id
| FApply (l1,l2) -> par (print_app lambda)
| FLabstract (id,l) -> par (print_abs lambda)

and print_app lambda =
match lambda with
| FApply (l1,l2) -> 
	(print_app l1)^" "^(print l2)
| _ -> print lambda 

and print_abs lambda = 
match lambda with
| FLabstract (id,l) ->
	lbda^id^"."^(print_abs l)
| _ -> print lambda

let p l = Printf.printf "%s \n" (print_app l)

let rec print_par = function
| FVar id -> id
| FApply (l1,l2) ->
	"("^(print_par l1)^" "^(print_par l2)^")"
| FLabstract (id,l) ->
	"("^lbda^id^"."^(print_par l)^")"

let p_par l = Printf.printf "%s \n" (print_par l)
