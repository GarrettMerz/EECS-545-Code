(*pp $PP *)

open Util

let use_stdin = ref false
let range = G.empty "range of lines"
let file = G.empty "file to print"

let parse = function
  | RE (digit+ as n1 : int) "-" (digit+ as n2 : int) -> 
      G.set range (n1, n2)
  | RE (digit+ as n : int) "-" eos ->
      G.set range (n, max_int)
  | RE (digit+ as n : int) eos ->
      G.set range (n, n)
  | RE "-" (digit+ as n : int) ->
      G.set range (1,n)
  | _ -> ()

let usage_msg = "\nUSAGE: lines [options] (n1-n2 | n1- | n) file\n\n Prints specified lines.\n"
let speclist = Arg.align
  [ ("-", Arg.Set use_stdin, " use standard input instead of file")]

let arg_error str =  
  Arg.usage speclist usage_msg;
  prerr_endline str;
  exit 1

let arg_pos = ref 0;;
Arg.parse speclist 
  (fun s -> 
     incr arg_pos;
     match !arg_pos with
       | 1 -> parse s
       | 2 -> G.set file s
       | _ -> arg_error "")
  usage_msg;;

if not (G.isdef range) then arg_error "range not set!\n";;
if not (G.isdef file) && not !use_stdin then arg_error "input not set!\n"

let first, last = G.get range
let e = Std.input_lines (if !use_stdin then stdin else open_in (G.get file));;
E.iteri 
  (fun i l -> 
     if (i + 1) >= first && (i + 1) <= last then
       print_endline l
     else if (i + 1) > last then exit 0)
  e

	     
