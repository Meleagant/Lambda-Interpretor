(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*            Pretty printer for lambda calculus           *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

open Ast


(** the 'λ' character  *)
let lamb = "λ"

(**
 * Add parenthesis around the string
 *)
let par str = "("^str^")"

let rec print lambda =
match lambda with
| FVar id  -> id
| FApply (l1,l2) -> print_left l1 ^" "^ (print_right l2)
| FLabstract (id,l) -> print_abs lambda

and print_left lamb =
match lamb with
| FVar id -> id
| FApply (l1, l2) -> print_left l1 ^" "^ (print_right l2)
| FLabstract (id, l) -> par (print_abs lamb)

and print_right lamb =
match lamb with
| FVar id -> id
| FApply (l1,l2) -> par (print_left l1 ^" "^ (print_right l2))
| FLabstract _ -> print_abs lamb

and print_app lambda =
match lambda with
| FApply (l1,l2) ->
	(print_app l1)^" "^(print l2)
| _ -> print lambda

and print_abs lambda =
match lambda with
| FLabstract (id,l) ->
	lamb^id^"."^(print l)
| _ -> print lambda

let p l = Printf.printf "%s\n" (print l)

let rec print_par = function
| FVar id -> id
| FApply (l1,l2) ->
	"("^(print_par l1)^" "^(print_par l2)^")"
| FLabstract (id,l) ->
	"("^lamb^id^"."^(print_par l)^")"

let p_par l = Printf.printf "%s" (print_par l)


(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*                 Printer for DB indices                  *)



let rec print_term = function 
  | WVar i -> Printf.sprintf "%d" i
  | WFreeVar id -> Printf.sprintf "%s" id
  | WApply (l1, l2) ->
    Printf.sprintf "%s %s" (print_left l1) (print_val l2)
  | WLTerme t -> Printf.sprintf "%s.%s" lamb (print_term t)

and print_val = function
  | WVar i -> Printf.sprintf "%d" i
  | WFreeVar id -> Printf.sprintf "%s" id
  | WApply (l1, l2) ->
    Printf.sprintf "(%s %s)" (print_left l1) (print_val l2)
  | WLTerme t -> Printf.sprintf "(%s.%s)" lamb (print_term t)

and print_left = function
  | WVar i -> Printf.sprintf "%d" i
  | WFreeVar id -> Printf.sprintf "%s" id
  | WApply (l1, l2) ->
    Printf.sprintf "%s %s" (print_left l1) (print_val l2)
  | WLTerme t -> Printf.sprintf "(%s.%s)" lamb (print_term t)

let print_DB t = 
  Printf.printf "%s\n" (print_term t)

(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

module H = Hashtbl
let env = H.create 17
and used = H.create 17

let new_id env =
let l_typ =
["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]
and compt = ref 1 in
let l_typ = List.map (fun x -> "'"^x) l_typ in
let curr = ref l_typ in
let rec aux curr =
	match curr with
	| t::q ->
		if H.mem env t then
			aux q
		else
			t
	| [] -> raise Not_found
and res = ref ""
and cont = ref true
in begin
	while !cont do
		try begin
			res := aux !curr;
			cont := false;
		end
		with
		| Not_found ->
		begin
			curr := List.map (fun x-> x^(string_of_int !compt)) l_typ;
			incr compt;
		end
	done;
	!res;
end




let rec print_ty env used t  =
match t with
| Basic i ->
	if H.mem env i then
		H.find env i
	else
		let new_n = new_id used
		in begin
			H.add env i new_n;
			H.add used new_n true;
			new_n;
		end
| App _ -> par (print_ty_app env used t)

and print_ty_app env used t =
match t with
| App (t1,t2) ->
	let p1 = print_ty env used t1
	and p2 = print_ty_app env used t2 in
	p1^" -> "^p2
| Basic _ ->
	print_ty env used t

let p_typ t =
	print_ty_app env used t
