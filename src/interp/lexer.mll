


{
	open Lexing
	open Parser

	exception Lexing_error of string

	let newline lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{pos with pos_lnum = pos.pos_lnum +1;
			 pos_bol = pos.pos_cnum }
}

let var  = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let lambda = '\x5c'

let ident = (var|digit| '_')+

let space = ' '|'\t'

rule next_tokens = parse
	| space+
		{next_tokens lexbuf}
	| lambda {LAMBDA}
	| '.' {DOT}
	| '(' {LPAR}
	| eof {EOF}
	| ')' {RPAR}
	| ident as s {IDENT s}
	|'\n' {newline lexbuf; next_tokens lexbuf}
	| "--" {comment lexbuf}
	| _ as c
		{raise (Lexing_error ("illegal character : "^ String.make 1 c))}

and comment = parse
	| '\n' {newline lexbuf; next_tokens lexbuf}
	| eof {EOF}
	| _ {comment lexbuf}
