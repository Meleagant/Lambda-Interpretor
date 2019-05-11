(*##################################################################*)
(*      Alpha,Beta,Eta transition for lambda-calculus               *)
(*##################################################################*)

open Ast
module PP = Printer

module Smap = Map.Make(String)

  (** [g] is "'" *) 
let g = String.make 1 '\x27'

let out = Array.mem "-v" Sys.argv

let alphabet = 
	["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"] 

(** [fv v t] returns [true] iff [v] is a free variable in [t] *)
let rec fv v = function
| FVar id -> id = v
| FApply (l1,l2) -> fv v l1 || fv v l2
| FLabstract (id,l) -> v != id && fv v l

(** [var lambda] returns a map containing all free variables in [lambda] *)
let rec var lambda = 
match lambda with 
| FVar id -> Smap.singleton id true 
| FApply (l1,l2) -> 
	Smap.merge (fun key a1 a2 -> Some true) (var l1) (var l2)
| FLabstract (id,l) ->
	Smap.remove id (var l) 

let free id la l2 = 
	let other = ref alphabet
	and used = Smap.merge (fun key a1 a2 -> Some true) (var la) (var l2)
	and cont = ref true
	and res = ref ""
	in
	let rec aux other = 
	match other with
	| [] -> raise Not_found
	| t:: q -> 
		if Smap.mem t used then
			aux q
		else
			t
	in begin
		while !cont do
			try begin
				res := aux !other;
				cont := false;
			end
			with
			| Not_found -> 
			other := List.map (fun x -> x^g) !other;
		done;
		!res;
	end

(* {1 Alpha-renommage } *)

  (** [replace (l1, l2) l] replace all occurences of [l1] by [l2] in [l] *)
let rec replace (l1, l2) l =
match l with
| l when l = l1 -> l2
| FVar id -> FVar id
| FApply (lApp1, lApp2) -> 
	FApply (replace (l1, l2) lApp1, replace (l1, l2) lApp2)
| FLabstract (id, la) ->
	match l1 with
	| FVar id1 when id = id1 -> 
	(* Si On redéfinit la nouvelle variable *)
		FLabstract (id, la)
	| _ when Smap.mem id (var l2) ->
	(* Cas de l'alpha-renommage *)
	let new_id = free id la l2
	in begin
        if out then
		    Printf.printf "    \027[94malpha-renommage :\027[0m %s => %s dans %s \n"
			id new_id (PP.print_abs l);
		FLabstract (new_id, replace (l1, l2) (replace (FVar id, FVar new_id) la));
	end
	| _ -> FLabstract (id, replace (l1, l2) la) 



(* {1 Beta-reduction } *)


(** We follow the leftmost outermost strategy *)
let rec beta l  =
match l with
| FVar id -> false,l
| FLabstract (id, la) -> 
	let b,lab = beta la in
	b, FLabstract (id, lab)
| FApply (FLabstract (id,l), l2) ->
    let _ = if out then
		  Printf.printf "  \027[92mBeta reduction\027[0m %s <- %s \n" 
      id (PP.print l2) in
		true,replace (FVar id, l2) l
| FApply (l1, l2) ->
		let b1, l1b = beta l1 in
		if b1 then
			b1, FApply (l1b, l2)
		else
			let b2, l2b = beta l2 in
			b2, FApply(l1, l2b)

let rec beta_wrapper l =
  let changed, l = beta l in
  if changed then 
    beta_wrapper l
  else
    l

let beta_prem l =
  let _ = Printf.printf "\n" in
  let _ = Printf.printf 
		"On commence la beta reduction sur : %s \n" (PP.print l) in
  let res = beta_wrapper l in
  let _ = Printf.printf "%s \n" (String.make  35 '=') in
  let _ = Printf.printf 
    "=> Fin de la beta reduction sur : %s \n" (PP.print res) in
  res


(* {1 Eta-reduction } *)

let rec eta l =

match l with
| FVar id -> false,l
| FLabstract (id,la) -> 
begin
	match la with
	| FApply (l1,l2) when l2 = FVar id && not (fv id l1) ->
	begin
        if out then
		    Printf.printf "- Eta reduction %s <- %s \n" (PP.print l) (PP.print l1);
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
		    "On commence la eta reduction sur : %s \n" (PP.print !res);
		Printf.printf "%s \n" (String.make  34 '=');
        while !cond do
			let b = eta !res 
			in begin
				cond :=  fst b;
				res := snd b;
				if !cond && out then
					PP.p !res;
			end;
		done;
		Printf.printf 
		    "=> Fin de la eta reduction sur : %s \n" (PP.print !res);
		!res;
	end
		















































