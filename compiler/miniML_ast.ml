

type ident = string

type binop = 
    | ADD | SUB | MULT | DIV
    | AND | OR

type unop = 
    | NOT

type cons = 
    | Int of int
    | Bool of bool

type expr = 
    | Const of cons
    | Var of ident
    | Call of ident*(expr list)
    | Binop of binop*expr*expr
    | Unop of unop*expr
    | If of expr*expr*expr
    | LetVar of ident*expr*expr
    (* let v = e in e *)
    | LetFun of ident*(ident list)*expr*expr
    (* let f id1 id2 = e in e *)
    | LetFunRec of ident*(ident list)*expr*expr
    (* let rec f id1 id2 = e in e *)
