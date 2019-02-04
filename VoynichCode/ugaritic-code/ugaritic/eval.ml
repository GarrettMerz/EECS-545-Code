#! /usr/bin/env ocamlscript
Ocaml.packs := ["ben"];
Ocaml.ocamlflags := ["-g"]
--
open Printf
open Util
open Std


(** computes the edit distance (Levenshtein) between two strings *)
let rec edit_dist (s1 : string) (s2 : string) =
  if s1 = "" then S.length s2 else 
    if s2 = "" then S.length s1 else
      let sub_cost = if s1.[0] = s2.[0] then 0 else 1 in
      let sub1, sub2 = S.slice ~first:1 s1, S.slice ~first:1 s2 in
      min (sub_cost + edit_dist sub1 sub2)
		  (min (1 + edit_dist sub1 s2) (1 + edit_dist s1 sub2))
;;

let eval (gold : string array) (predict : string array) = 
   let total1      = ref 0 in
   let sed1        = ref 0 in
   let correct1    = ref 0 in
   let total2      = ref 0 in
   let sed2        = ref 0 in
   let correct2    = ref 0 in
   A.switer2 gold predict (fun g p ->
      match split g with
         | c :: _ :: rest ->
            if L.length rest > 0 then 
               begin
                  let rest = L.smap rest (fun s ->
                     let res = try fst (S.split s "/") with ExtString.Invalid_string -> s in
                     S.remove '(' (S.remove ')' (S.remove '-' res)))
                  in
                  let eds = L.map (edit_dist p) rest in
                  let ed = min_item (aol eds) in
                  let count = ios c in
                  total1   += 1;
                  sed1     += ed;
                  if L.mem p rest then 
                     correct1 += 1; 
                  total2   += count;
                  sed2     += count * ed;
                  if L.mem p rest then
                     correct2 += count
               end
         | _ -> failwith "malformed gold file");
   brintf "avg sed 1       :  %f" (foi !sed1 /. foi !total1);
   brintf "pct correct 1   :  %f" (foi !correct1 /. foi !total1);
   brintf "avg sed 2       :  %f" (foi !sed2 /. foi !total2);
   brintf "pct correct 2   :  %f" (foi !correct2 /. foi !total2)   
;;
            
let main () = 
   let gold_file = "words.fresh" in
   let out_file = G.empty "output file to evaluate" in
   let speclist = Arg.align [] in
   let usage_msg = "eval.ml <eval_file>" in
   let arg_error str = 
      Arg.usage speclist usage_msg;
      prerr_endline str;
      exit 2 in
   let arg_pos = ref 0 in
   Arg.parse speclist
      (fun s ->
         incr arg_pos;
         match !arg_pos with
            | 1 -> G.set out_file s
            | _ -> arg_error "too many args!")
      usage_msg;
   if G.notdef out_file then
      arg_error "** must specify file to evaluate";
   eval (lines_of_file gold_file) (lines_of_file $ G.get out_file)
;;

main ()
       