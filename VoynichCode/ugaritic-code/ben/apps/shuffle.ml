(*pp $PP *)

open Util

let seed = G.empty "Seed for PRG"
let use_stdin = ref false
let in_file = G.empty "Input file"
let out_file = G.empty "Output file"
let in_place = ref false

let usage_msg = "\nUSAGE: shuffle [options] file\n"
let speclist = Arg.align
  [ ("-s", Arg.Int (fun i -> G.set seed i), " seed for PRG (default uses system clock)");
    ("-", Arg.Set use_stdin, " use standard input instead of file");
    ("-o", Arg.String (fun s -> G.set out_file s), " print to file (instead of stdout)");
    ("-i", Arg.Set in_place, " in-place shuffle (i.e. overwrite input file)")
  ]
  
let arg_error str = 
  Arg.usage speclist usage_msg;
  prerr_endline str;
  exit 2;;

Arg.parse speclist (fun s -> G.set in_file s) usage_msg;;

if not (G.isdef in_file || !use_stdin) then arg_error "";;
let in_ch = if !use_stdin then stdin else open_in (G.get in_file)
let a = array_of_input in_ch;;
if A.length a = 0 then failwith "Can't shuffle an empty file!";;
if not (G.isdef seed) then Random.self_init ();;
A.shuffled_in ~seed:(G.opt seed) a;;
let out_ch = if G.isdef out_file then open_out (G.get out_file) else
  if !in_place && G.isdef in_file then open_out (G.get in_file) else stdout;; 
output_array out_ch a

