(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)
(*                           Typeur for \-calculus                        *)
(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

open Ast
open Printer

module H = Hashtbl



let compt = ref 0

let new_type () = 
begin
	incr compt;
	Basic !compt;
end

exception RecType of ty*ty

type env = (int,ty) H.t

let inR t1 t2 = 
(* Check if t1 R t2 *)
	()

let rec enrichie t1 t2 = 
match t1,t2 with
| Basic i1, Basic i2 -> Basic i1
| App (t1g,t1d), App (t2g,t2d) ->
	App (enrichie t1g t2g,enrichie t1d t2d)
| App _ , Basic _ -> t1
| _ -> t2

let replace env t nt = ()

let rec typable t l env = 
match l with
| FVar id ->
begin
	if not (H.mem env id) then
		H.add env id (new_type () );
	let tl = H.find env id 
	in begin
		inR t tl;
		let new_t = enrichie t tl
		in begin
			replace env t new_t;
			replace env tl new_t;
			new_t;
		end;
	end;
end
| FApply (l1,l2) ->
begin
	let t2 = typable (new_type () ) l2 env in
	let proto_t1 = App (t2,t) in
	let t1 = typable proto_t1 l1 env in
	match t1 with
	| App (tg,td) -> td 
	| _ -> assert false
end
| FLabstract (id,la) -> 
	let tg,td = 
		match t with
		| Basic i -> new_type (),new_type ()
		| App (tg,td) -> (tg,td)
	in
	let new_env  = H.copy env 
	in begin
		H.add new_env id tg;
		let t_res = typable td la new_env in
		let t_tot = App(H.find new_env id,t_res) in
		let t_fin = enrichie t_tot t
		in begin
			inR t t_fin;
			replace env t t_fin ;
			t_fin;
		end;
	end;













