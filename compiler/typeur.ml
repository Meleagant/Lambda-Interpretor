
open MiniML_ast

module Smap = Map.Make(String)
type typ = 
    | Int
    | Bool
    | FUN of typ*typ

type env = typ Smap.t


let rec typing_e env = function
| Const c ->
begin
    match c with
    | Int _ -> Int
    | Bool _ -> Bool
end

| Var id -> 
begin
    if Smap.mem id env then
        Smap.find id env
    else
        raise (UndifinedVar (id))
end

| Call _ -> assert false

| Binop (b,e1,e2) ->
begin
    let t1 = typing_e env e1
    and t2 = typing_e env e2 in
    match b with
    | ADD | SUB | MULT | DIV | REM ->
    if t1 <> Int then
        assert false
    else if t2 <> Int then
        assert false
    else
        Int
    | AND | OR ->
    if t1 <> Bool then
        assert false
    else if t2 <> Bool then
        assert false
    else
        Bool
    | _ ->
    if t1 <> t2 then
        assert false
    else
        Bool
end

| Unop (u,e) ->
begin
    let t = typing_e env e in
    if t <> Bool then
        assert false
    else
        Bool
end

| If (c,e1,e2) ->
begin
    let tc = typing_e env c
    and t1 = typing_e env e1
    and t2 = typing_e env e2
    in
    if tc <> Bool then
        assert false
    else
        if t1 <> t2 then
            assert false
        else
            t1
end

| LetVar (id,evar,e) ->
begin
    let tvar = typing_e env evar in
    let new_env = Smap.add id tvar env in
    typing_e new_env e
end

| LetFun _ -> assert false

| LetFunRec _ -> assert false







let typing_ast ast =
    typing_e (Smap.empty) ast






