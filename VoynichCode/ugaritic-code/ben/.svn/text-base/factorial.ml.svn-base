#! /usr/bin/env ocamlscript
Ocaml.packs := ["num"];
Ocaml.ocamlflags := [ "-g" ]
--

open Num
open Format

let factorial n = 
	if n </ Int 0 then failwith "factorial undefined in inputs < 0";
	let rec f n acc = 
		match n with
		| Int 0 -> acc
		| Int 1 -> acc
		| n -> f (n -/ Int 1) (acc */ n)
	in
	f n (Int 1)
;;

let n1 = factorial (Int (int_of_string Sys.argv.(1))) in
let n2 = factorial (Int (int_of_string Sys.argv.(2))) in
print_endline (string_of_num (n1 // n2))


