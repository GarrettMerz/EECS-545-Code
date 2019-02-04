open Util

let use_stdin = ref false
let in_file = G.empty "Input file"
let out_file = G.empty "Output file"
let rate = G.empty "print rate"
let start = ref 1
let in_place = ref false

let usage_msg = "\nUSAGE: skip_lines [options] file\n"
let speclist = Arg.align
	[ 	("-", Arg.Set use_stdin, " use standard input instead of file");
		("-o", Arg.String (G.set out_file), " print to file (instead of stdout)");
		("-n", Arg.Int (G.set rate), " <n> print every <n>th line");
		("-s", Arg.Set_int start, " <s> start with <s>th line (default 1st)")]
	
let arg_error str = 
	Arg.usage speclist usage_msg;
	prerr_endline str;
	exit 2;;

Arg.parse speclist (fun s -> G.set in_file s) usage_msg;;

if not (G.isdef rate) then arg_error "must set rate!!";;
if not (G.isdef in_file || !use_stdin) then arg_error "";;
decr start;;

let in_ch = if !use_stdin then stdin else open_in (G.get in_file)
let n = G.get rate;;

let lines = array_of_input in_ch;;

A.iteri
	(fun i line -> 
		if i >= !start && (i - !start) mod n  = 0 then print_endline line)
	lines