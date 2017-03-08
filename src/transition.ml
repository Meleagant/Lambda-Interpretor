(*##################################################################*)
(*      Alpha,Beta,Eta transition for lambda-calculus               *)
(*##################################################################*)

open Ast
open Printer

module Smap = Map.Make(String)





let rec fv v = function
(*
tell if v is a free variable 
*)
| FVar id -> id = v
| FApply (l1,l2) -> fv v l1 || fv v l2
| FLabstract (id,l) -> v != id && fv v l


let rec replace (l1,l2) = function
(*
replace l1 by l2 in l
*)
| l when l = l1 -> l2
| FVar id -> FVar id
| FApply (lApp1,lApp2) -> 
	FApply (replace (l1,l2) lApp1,replace (l1,l2) lApp2)
| FLabstract (id,l) ->
	match l1 with
	| FVar id1 when id = id1 ->
		FLabstract (id,l)
	| _ ->
		FLabstract (id,replace (l1,l2) l)

let rec alpha env = function
| FVar id -> ()
| FApply (l1,l2) -> ()
| FLabstract (id,l) -> ()


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
		Printf.printf "Beta reduction %s => %s \n" id (print l2);
		true,replace (FVar id,l2) l;
	end


let beta_prem l = 
	let cond = ref true 
	and res = ref l 
	in begin
		Printf.printf "\n";
		Printf.printf 
			"On commence la beta reduction sur %s \n" (print !res);
		Printf.printf "%s \n" (String.make  33 '=');
		while !cond do
			let b = beta !res 
			in begin
				cond :=  fst b;
				res := snd b;
				if !cond then
					p !res;
			end
		done;
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
		Printf.printf "Eta reduction %s => %s \n" (print l) (print l1);
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
			"On commence la eta reduction sur %s \n" (print !res);
		Printf.printf "%s \n" (String.make  32 '=');
		while !cond do
			let b = eta !res 
			in begin
				cond :=  fst b;
				res := snd b;
				if !cond then
					p !res;
			end
		done;
		!res;
	end
		















































