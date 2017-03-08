(*###########################################################################*)
(*              main file  *)
(*###########################################################################*)

open Parser
open Lexing
open Format
open Transition
open Printer

let file_name = ref " "
and parse_only = ref false


let spec = 
	["--parse-only", Arg.Set parse_only, " stop after parsing"]

let usage = "usage: main [optioon] file.lamb"


let file = 
	let file = ref None in
	let set_file s = 
		if not (Filename.check_suffix s ".lamb") then
			raise (Arg.Bad "no .lamb extension");
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
		let lambda = ref (Parser.file Lexer.next_tokens lb) 
		in begin
			p !lambda;
			if !parse_only then
				exit 0
			else
			begin
				lambda := beta_prem !lambda;
				lambda := eta_prem !lambda;
				exit 0;
			end;
		end
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
	







































