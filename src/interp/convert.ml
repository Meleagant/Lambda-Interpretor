(*###########################################################################*)
(*###########################################################################*)

open Ast
module Smap = Map.Make(String)

type free = (string list)*int

let basicList = ["r";"s";"t";"u";"v";"w";"x";"y";"z"]


let first_to_work lambda = 
	let rec aux env offset lambda = 
	match lambda with
	| FVar id -> 
		let x = Smap.find id env 
		in 
		WVar x 
	| FApply (l1,l2) ->
		let l1W = aux env (offset +1) l1
		and l2W = aux env (offset +1) l2 
		in
		WApply (l1W,l2W)
	| FLabstract(id,l) ->
		let new_env = Smap.add id offset env
		in
		WLTerme (aux new_env (offset+1) l)
	in
	aux Smap.empty 0 lambda
		

let rec extractFree free = 
match fst !free with
| [] -> 
begin
	free := (basicList,snd !free +1);
	extractFree free;
end
| t::q ->
let lvl = snd !free 
in begin
	free := (q,lvl);
	if lvl = 0 then
		t
	else
		t^(string_of_int lvl)
end


(*
TODO finish that :
let work_to_first lambda =
	let rec aux offset free lambda

*)





















































