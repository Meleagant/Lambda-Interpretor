(*################################################*)
(*                Lexer for mini-ML               *)
(*################################################*)

{ 
    open Lexing
    open Parser

    exception Lexing_error of string 
	
	let newline lexbuf = 
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}

    let kwd_tbl = [
        "let", LET;
        "in", IN;
        "rec", REC;
        "and", AND;
        "if", IF;
        "then", THEN;
        "else", ELSE;
        "true", TRUE;
        "false", FALSE;
        "int", INT;
        "bool", BOOL ]

    let id_or_kwd s = 
        try List.assoc s kwd_tbl
        with _ -> IDENT s

}


let alpha = ['a'-'z' 'A'-'Z']
let chiffre = ['0'-'9']
let ident = alpha ( alpha | chiffre | '_')*
let integer = chiffre +
let space = ' ' | '\t'

rule next_tokens = parse
    | eof {EOF}
    | ident as s {id_or_kwd s}
    | integer as s { CONST (Int (int_of_string s))}
    | space {next_tokens lexbuf}
    | '\n' {newline lexbf; next_tokens lexbuf}
    | '+' {PLUS}
    | '-' {SUB}
    | '*' {TIMES}
    | '/' {DIV}
    | '%' {REM}
    | '=' {EQUAL}
    | "!=" {DIFF}
	| '(' {LPAR}
	| ')' {RPAR}
    | "||" {OR}
    | "&&" {AND}

	| _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

