(*###########################################################################*)
(*###########################################################################*)

open Ast
module Smap = Map.Make(String)

(** An environment maps to each varaible the level where the variable was
 * introduced *)
type environment = int Smap.t

let lamb : string = "λ"


let rec first_to_DB depth env = function
 | FVar id when Smap.mem id env -> 
   let dec_depth = Smap.find id env in
   WVar (depth - dec_depth)
 | FVar id -> 
   WFreeVar id
 | FApply (l1, l2) -> 
   let l1' = first_to_DB depth env l1
   and l2' = first_to_DB depth env l2 in
   WApply (l1',l2')
 | FLabstract (id, t) ->
   let new_depth = depth +1 in
   let new_env = Smap.add id new_depth env in
   WLTerme (first_to_DB new_depth new_env t)

let convert = first_to_DB 0 Smap.empty


