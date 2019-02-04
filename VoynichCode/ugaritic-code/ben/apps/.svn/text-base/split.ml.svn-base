(*pp $PP *)

open Util

type split_pattern = string list * float list
let use_stdin = ref false
let pattern : split_pattern G.t = G.empty "split pattern"
let file = G.empty "file to split"

let parse s =  
  let pieces = S.nsplit s ":" in
  let parse_piece = function
    | RE (_* as name) ("." digit+ as pct : float) eos -> name, pct
    | _ -> raise (Invalid_argument ("couldn't parse " ^ s)) in
  let names, pcts = L.split (L.map parse_piece pieces) in
  if not (L.fold_left ( +. ) 0. pcts = 1.0) then 
    raise (Invalid_argument "floats don't add to 1!");
  if not (L.for_all (fun f -> f < 1. && f > 0.) pcts) then
    raise (Invalid_argument "floats not all between 0 and 1!");
  G.set pattern (names, pcts)
  
let usage_msg = 
  "\nUSAGE: split [options] pattern file\n\n Split 'file' into 'pattern.'\n\n" 
  ^ " pattern    := expression:pattern | pattern\n"
  ^ " expression := <name><float>\n"
  ^ " <name> is the output file name, and <float> is a fraction of the input file.\n\n"
  ^ " E.g., split train.9:test.1 all\n"
    
    
    
let speclist = Arg.align
  [("-", Arg.Set use_stdin, " use standard input instead of file")]

let arg_error str =  
  Arg.usage speclist usage_msg;
  prerr_endline str;
  exit 1

let arg_pos = ref 0;;
Arg.parse speclist
  (fun s ->
     incr arg_pos;
     match !arg_pos with
       | 1 -> (try parse s with Invalid_argument s -> arg_error s)
       | 2 -> G.set file s
       | _ -> arg_error "too many arguments")
  usage_msg;;

if not (G.isdef pattern) then arg_error "pattern not set!\n";;
if not (G.isdef file || !use_stdin) then arg_error "input not set!\n";;

let names, pcts = G.get pattern
let names, pcts = A.of_list names, A.of_list pcts
let lines = array_of_input (if !use_stdin then stdin else (open_in (G.get file)))

let split_lines lines pcts : string array array =
  let len = A.length lines in
  let len' = float len in
  let all = ref 0 in
  let lengths = A.map 
    (fun pct -> 
       let res = int_of_float (len' *. pct) in 
       all += res; res)
    pcts in
  let remainder = len - !all in
  assert (remainder >= 0);
  for i = 1 to remainder do
    let idx = i mod len in
    lengths.(idx) <- lengths.(idx) + 1
  done;
  let sum = A.fold_left ( + ) 0 lengths in
  assert (sum = len);
  let e = A.enum lines in
  A.map 
    (fun n -> 
       let a = A.make n "" in
       for i = 0 to n - 1 do
	 match E.get e with
	   | Some ln -> a#(i) <- ln
	   | _ -> assert false
       done;
       a)
    lengths in

A.iteri 
  (fun i lines -> file_of_array names#(i) lines)
  (split_lines lines pcts)
  

    
    
