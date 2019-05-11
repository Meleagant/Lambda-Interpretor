(*###########################################################################*)
(*              main file  *)
(*###########################################################################*)

open Lamb_parser
open Lexing
open Format
open Transition
open Printer
open Convert
open Lamb_typer

let file_name = ref " "
and parse_only = ref false
and type_only = ref false
and force_exec = ref false
and out = ref false


let spec =
	["--parse-only", Arg.Set parse_only, " stop after parsing";
	 "--type-only",Arg.Set type_only," stop after typing";
     "--force-exec",Arg.Set force_exec," force the execution ";
     "-v", Arg.Set out, "\t Display all the beta-eta réduction "]

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
	printf "File \"%s\", line %d, coracter %d-%d: \n" file l fc lc

let () =
	let chan = open_in file in
	let lb = Lexing.from_channel chan in
	try
		let lambda = ref (Lamb_parser.file Lamb_lexer.next_tokens lb)
		in begin
			Printf.printf "On travaille sur :  \n";
			Printf.printf "================== \n";
			p !lambda;
      let _ = 
      begin
        Printf.printf "Ac Indices de De Bruijn\n";
        print_DB (convert !lambda);
      end in
      if !parse_only then
				exit 0
			else
			begin
				Printf.printf "\n";
				Printf.printf "Typage : \n";
				Printf.printf "======== \n";
				if typage !lambda then
					if !type_only then
						exit 0
					else
					begin
						lambda := beta_prem !lambda;
						lambda := eta_prem !lambda;
            Printf.printf "\n";
            Printf.printf "Résultat : \n";
            Printf.printf "========== \n";
            p !lambda;
            Printf.printf "typage :\n";
            Printf.printf "--------\n";
						assert (typage !lambda);
						exit 0;
					end
				else
					if !force_exec then
					begin
						lambda := beta_prem !lambda;
						lambda := eta_prem !lambda;
            Printf.printf "\n";
            Printf.printf "Résultat : \n";
            Printf.printf "========== \n";
            p !lambda;
						exit 0;
					end
					else
						exit 0
			end;
		end
	with
	| Lexer.Lexing_error s ->
	print_string "CACA \n";
	report (lexeme_start_p lb , lexeme_end_p lb);
	printf "lexical error %s \n" s;
	exit 1
	| Lamb_parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	printf "syntax error \n";
	exit 1
	| _ ->
	printf "CACA \n";
	exit 1
