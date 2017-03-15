

type pos = Lexing.position*Lexing.position



type ident = string

type binop = 
    | ADD | SUB | MULT 
    | DIV | REM
    | AND | OR 
    | EQUAL | DIFF

type unop = 
    | NOT

type cons = 
    | Int of int
    | Bool of bool

type expr = 
    | Const of cons*pos
    | Var of ident*pos
    | Call of ident*(expr list)*pos
    | Binop of binop*expr*expr*pos
    | Unop of unop*expr*pos
    | If of expr*expr*expr*pos
    | LetVar of ident*expr*expr*pos
    (* let v = e in e *)
    | LetFun of ident*(ident list)*expr*expr*pos
    (* let f id1 id2 = e in e *)
    | LetFunRec of ident*(ident list)*expr*expr*pos
    (* let rec f id1 id2 = e in e *)


exception UndifinedVar of ident*pos
exception TypingError of string*string*pos
