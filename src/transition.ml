(*##################################################################*)
(*      Alpha,Beta,Eta transition for lambda-calculus               *)
(*##################################################################*)

open Ast
open Printer

module Smap = Map.Make(String)

let g = String.make 1 '\x27'



let rec fv v = function
(*
tell if v is a free variable 
*)
| FVar id -> id = v
| FApply (l1,l2) -> fv v l1 || fv v l2
| FLabstract (id,l) -> v != id && fv v l

let rec var lambda = 
match lambda with 
| FVar id -> Smap.singleton id true 
| FApply (l1,l2) -> 
	Smap.merge (fun key a1 a2 -> Some true) (var l1) (var l2)
| FLabstract (id,l) ->
	Smap.merge (fun key a1 a2 -> Some true) (Smap.singleton id true) (var l)

(*##################################################################*)
(*                         alpha-reduction                          *)
(*##################################################################*)

let rec replace (l1,l2) l =
(*
replace l1 by l2 in l
*)
match l with
| l when l = l1 -> l2
| FVar id -> FVar id
| FApply (lApp1,lApp2) -> 
	FApply (replace (l1,l2) lApp1,replace (l1,l2) lApp2)
| FLabstract (id,la) ->
	match l1 with
	| FVar id1 when id = id1 -> 
	(* Si On redéfinit la nouvelle variable *)
		FLabstract (id,la)
	| _ when Smap.mem id (var l2) ->
	let new_id = id^g
	in begin
		Printf.printf "/!%s On fait un alpha-renommage : %s => %s dans %s \n"
			lbda id new_id (print_abs l);

		FLabstract (new_id,replace (l1,l2) (replace (FVar id,FVar new_id)
		la));
	end
	| _ -> FLabstract (id,replace (l1,l2) la) 



(*##################################################################*)
(*                          Beta-reduction                          *)
(*##################################################################*)


let rec beta l  =
(*
We follow the leftmost outermost strategy
*)
match l with
| FVar id -> false,l
| FLabstract (id,la) -> 
	let b,lab = beta la in
	b,FLabstract(id,lab)
| FApply (l1,l2) ->
	match l1 with
	| FVar _ | FApply _ -> 
		let b1,l1b = beta l1
		and b2,l2b = beta l2 in
		b1||b2,FApply(l1b,l2b)
	| FLabstract (id,l) ->
	begin
		Printf.printf "- Beta reduction %s <- %s \n" id (print l2);
		true,replace (FVar id,l2) l;
	end


let beta_prem l = 
	let cond = ref true 
	and res = ref l 
	in begin
		Printf.printf "\n";
		Printf.printf 
			"On commence la beta reduction sur : %s \n" (print_app !res);
		Printf.printf "%s \n" (String.make  35 '=');
		while !cond do
			let b = beta !res 
			in begin
				cond :=  fst b;
				res := snd b;
				if !cond then
					p !res;
			end;
			Printf.printf "\n";
		done;
		Printf.printf 
			"=> Fin de la beta reduction sur : %s \n" (print_app !res);
		Printf.printf "\n";
		!res;
	end
		

(*##################################################################*)
(*                           Eta-reduction                          *)
(*##################################################################*)

let rec eta l =

match l with
| FVar id -> false,l
| FLabstract (id,la) -> 
begin
	match la with
	| FApply (l1,l2) when l2 = FVar id && not (fv id l1) ->
	begin
		Printf.printf "- Eta reduction %s <- %s \n" (print l) (print l1);
		true,l1;
	end
	| _ -> 
		let b,lae = eta la in
		b,FLabstract (id,lae)
end
| FApply (l1,l2) ->
	let b1,l1e = eta l1
	and b2,l2e = eta l2 in
	b1||b2,FApply(l1e,l2e)
	
let eta_prem l = 
	let cond = ref true 
	and res = ref l 
	in begin
		Printf.printf "\n";
		Printf.printf 
			"On commence la eta reduction sur : %s \n" (print_app !res);
		Printf.printf "%s \n" (String.make  34 '=');
		while !cond do
			let b = eta !res 
			in begin
				cond :=  fst b;
				res := snd b;
				if !cond then
					p !res;
			end;
			Printf.printf "\n";
		done;
		Printf.printf 
			"=> Fin de la eta reduction sur : %s \n" (print_app !res);
		Printf.printf "\n";
		!res;
	end
		















































