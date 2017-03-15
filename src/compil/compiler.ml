
open MiniML_ast
open Format

module Smap = Map.Make(String)

type env = string Smap.t

let lamb = String.make 1 '\x5c'
let succ = sprintf "%sn.%sf.%sx.f (n f x)" lamb lamb lamb
let add = sprintf "%sm.%sn.m (%s) n" lamb lamb succ
let mult = sprintf "%sm.%sn.%sf. m (n f)" lamb lamb lamb




let compile_int x = 
	let res = ref "x"
	in begin
		for i = 1 to x do
			res := sprintf "f (%s)" !res
		done;
		sprintf "(%sf.%sx.%s)" lamb lamb !res;
	end
		

let rec compile_e env = function
| Const (c,_) ->
begin
	match c with
	| Int x -> compile_int x
	| Bool b -> 
		if b then
			sprintf "%sa.%sb.a" lamb lamb
		else
			sprintf "%sa.%sb.b" lamb lamb
end
| Var (id,_) ->
	Smap.find id env

| Call _ -> assert false

| Binop (b,e1,e2,_) ->
begin
	let s1 = compile_e env e1
	and s2 = compile_e env e2 in
	match b with
	| ADD -> 
		sprintf "(%s) %s %s" add s1 s2
	| _ -> assert false
end

| _ -> assert false

let compile e = 
	compile_e (Smap.empty) e


