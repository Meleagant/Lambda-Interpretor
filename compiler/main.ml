
open Lexing
open Parser
open Format
open Typeur
open Compiler


let file_name = ref " "
and parse_only = ref false
and type_only = ref false

let spec = 
	["--parse-only", Arg.Set parse_only, " stop after parsing";
     "--type-only",Arg.Set type_only," stop after typing"]

let usage = "usage: main [optioon] file.mml"

let file = 
	let file = ref None in
	let set_file s = 
		if not (Filename.check_suffix s ".mml") then
			raise (Arg.Bad "no .mml extension");
		file := Some s
	in begin
		Arg.parse spec set_file usage;
		match !file with
		|Some f -> f
		|None -> Arg.usage spec usage; exit 1
	end

let report (b,e) = 
	let l = b.pos_lnum in
	let fc = b.pos_cnum - b.pos_bol + 1 in
	let lc = e.pos_cnum - b.pos_bol + 1 in
	eprintf "File \"%s\", line %d, coracter %d-%d: \n" file l fc lc


let () = 
	let chan = open_in file in
	let lb = Lexing.from_channel chan in
	try 
		let ast = (Parser.file Lexer.next_tokens lb) 
        in begin
            if !parse_only then
                exit 0
            else
            begin
                typing_ast ast;
                if !type_only then
                    exit 0
                else
                let lamb = compile ast in
                let file_out = (String.sub file 0 (String.length file -4))^".lamb" in
                let out = open_out file_out 
                in begin
                    output_string out lamb;
                    close_out out;
                    exit 0;
                end;
            end;
        end;
	with
	| Lexer.Lexing_error s -> 
	print_string "CACA \n";
	report (lexeme_start_p lb , lexeme_end_p lb);
	eprintf "lexical error %s \n" s;
	exit 1
	| Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error \n";
	exit 1
	| _ -> 
	eprintf "CACA \n";
	exit 1
	








                    











































