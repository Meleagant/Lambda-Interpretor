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

let rec noSubTyp t tApp = 
match tApp with
| App (t1,t2) when t = t1 || t = t2 -> 
	raise (RecursiveType (t,tApp))
| App (t1,t2) -> 
	begin
		noSubTyp t t1; 
		noSubTyp t t2;
	end
| Basic _ -> ()

let rec inR t1 t2 = 
(* Check if t1 R t2 *)
match t1,t2 with
| Basic _ , Basic _ -> ()
| Basic _  , App _ ->
	noSubTyp t1 t2
| App _ , Basic _ ->
	inR t2 t1
| App (t1g,t1d), App (t2g,t2d) -> ()

let rec proto_print t =
match t with
| Basic i -> string_of_int i
| App (t1,t2) ->
	"("^(proto_print t1)^" -> "^(proto_print t2)^")"

let print_env env = 
	let aux id t = 
	Printf.printf "L'ident %s est de type %s \n" id (proto_print t)
	in
	H.iter aux env


let rec enrichie t1 t2 = 
match t1,t2 with
| Basic i1, Basic i2 -> Basic i1
| App (t1g,t1d), App (t2g,t2d) ->
	App (enrichie t1g t2g,enrichie t1d t2d)
| App _ , Basic _ -> t1
| _ -> t2



let replace env t nt =
	let rec aux t_previous = 
		if t = t_previous then
			nt
		else match t_previous with
		| Basic _ -> t_previous
		| App (t1,t2) ->
			App (aux t1,aux t2) 
	in begin
	H.iter (fun id t -> H.replace env id (aux t)) env;
	end

let merge env new_env =
	H.iter (fun id t -> H.replace env id t) new_env


let rec typable t l env = 
match l with
| FVar id ->
begin
	if not (H.mem env id) then
		H.add env id (new_type () );
	let tl = H.find env id 
	in begin
		inR t tl;
		let new_t = enrichie tl t
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
begin
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
		let t_fin = enrichie t t_tot 
		in begin
			inR t t_fin;
			merge env new_env;
			replace env t t_fin ;
			t_fin;
		end;
	end;
end

let typage l = 
begin 
	try 
	begin
		let t = typable (new_type ()) l (H.create 17) in
		Printf.printf "Le type de l'expression est : %s \n" (p_typ t);
		true;
	end
	with RecursiveType (t1,t2) ->
	begin
		Printf.printf "\027[31m! %s ! \027[0m\n" (String.make 25 '-');
		Printf.printf "On observe un type recursif \n";
		Printf.printf "%s := %s \n" (p_typ t1) (p_typ t2);
		Printf.printf "\027[31mTyping FAILED !!!!! \027[0m\n \n";
		false;
	end
end









