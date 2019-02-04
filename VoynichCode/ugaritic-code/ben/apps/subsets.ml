(*pp  $PP *)

open Util

module HS = HashSet

let num = ref 2
let d = ref " "
let f = ref None
let use_stdin = ref false
  
let usage_msg = "\nUSAGE: subsets [options] [file]\n\n  Prints all subsets of size n.\n"
let speclist = Arg.align
  [ ("-d", Arg.Set_string d, " <string> Delimiter (default = ' ')");
    ("-n", Arg.Set_int num, " <int> Subset size (default = 2)");
    ("-", Arg.Set use_stdin, " use standard input instead of file")]
  
let arg_error str =
  Arg.usage speclist usage_msg;
  prerr_endline str;
  exit 1;;

Arg.parse speclist (fun s -> f := Some s) usage_msg;;

if !num <= 1 then arg_error "";;

if Opt.is_none !f && not !use_stdin then arg_error "";;
let l = 
  if !f = None then L.of_enum (Std.input_lines stdin)
  else list_of_file (Opt.get !f);;

let rec f n e l =
  match l with
    | [] -> ()
    | h :: t ->
	if n + 1 = !num then
	  print_endline (e ^ !d ^ h)
	else
	  f (n + 1) (e ^ !d ^ h) t;
	f n e t;;

let rec g = function
    | [] -> ()
    | h :: t ->
	f 1 h t;
	g t;;
g l









