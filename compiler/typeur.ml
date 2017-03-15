
open MiniML_ast

module Smap = Map.Make(String)
type typ = 
    | Int
    | Bool
    | FUN of typ*typ

type env = typ Smap.t

let rec print_typ = function
| Int -> "int"
| Bool -> "bool"
| FUN (t1,t2) -> Format.sprintf "%s -> %s" (print_typ t1) (print_typ t2)

let extract_pos = function
| Const (_,p) -> p
| Var (_,p) -> p
| Call (_,_,p) -> p
| Binop (_,_,_,p) -> p
| Unop (_,_,p) -> p
| If (_,_,_,p) -> p
| LetVar (_,_,_,p) -> p
| LetFun (_,_,_,_,p) -> p
| LetFunRec (_,_,_,_,p) -> p

let rec typing_e env = function
| Const (c,p) ->
begin
    match c with
    | Int _ -> Int
    | Bool _ -> Bool
end

| Var (id,p) -> 
begin
    if Smap.mem id env then
        Smap.find id env
    else
        raise (UndifinedVar (id,p))
end

| Call _ -> assert false

| Binop (b,e1,e2,p) ->
begin
    let t1 = typing_e env e1
    and t2 = typing_e env e2 in
    match b with
    | ADD | SUB | MULT | DIV | REM ->
    if t1 <> Int then
        raise (TypingError (print_typ t1,print_typ Int,extract_pos e1)) 
    else if t2 <> Int then
        raise (TypingError (print_typ t2,print_typ Int,extract_pos e2)) 
    else
        Int
    | AND | OR ->
    if t1 <> Bool then
        raise (TypingError (print_typ t1,print_typ Bool,extract_pos e1)) 
    else if t2 <> Bool then
        raise (TypingError (print_typ t2,print_typ Bool,extract_pos e2)) 
    else
        Bool
    | _ ->
    if t1 <> t2 then
        raise (TypingError (print_typ t1,print_typ t2,p)) 
    else
        Bool
end

| Unop (u,e,p) ->
begin
    let t = typing_e env e in
    if t <> Bool then
        raise (TypingError (print_typ t,print_typ Bool,extract_pos e)) 
    else
        Bool
end

| If (c,e1,e2,p) ->
begin
    let tc = typing_e env c
    and t1 = typing_e env e1
    and t2 = typing_e env e2
    in
    if tc <> Bool then
        raise (TypingError (print_typ tc,print_typ Bool,extract_pos c)) 
    else
        if t1 <> t2 then
        	raise (TypingError (print_typ t1,print_typ t2,p)) 
        else
            t1
end

| LetVar (id,evar,e,p) ->
begin
    let tvar = typing_e env evar in
    let new_env = Smap.add id tvar env in
    typing_e new_env e
end

| LetFun _ -> assert false

| LetFunRec _ -> assert false







let typing_ast ast =
    typing_e (Smap.empty) ast






