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




(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

module H = Hashtbl


let new_id env = 
let l_typ =  
["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"] 
and compt = ref 1 
in
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
	print_ty_app (H.create 17) (H.create 17) t

















