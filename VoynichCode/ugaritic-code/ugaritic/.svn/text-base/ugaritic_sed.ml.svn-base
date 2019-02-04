#! /usr/bin/env ocamlscript
Ocaml.packs := ["ben";"gsl";"hhl"];
Ocaml.ocamlflags := ["-g"]
--
open Printf
open Util
open Std

let rng = Gsl_rng.make (Gsl_rng.default ());;
Random.self_init ();;
let seed = Random.nativeint (Nativeint.of_int max_int);;
Gsl_rng.set rng seed;;

type edit = Ins of int | Del of int | Sub of (int * int) | End
type model = {
   p_lang         : string;
   c_lang         : string;
   p_chars        : char array;
   c_chars        : char array;
   p_idx          : (char, int) Ht.t;
   c_idx          : (char, int) Ht.t;
   p_words        : int array array;
   c_words        : int array array;
   p_counts       : float array;
   c_counts       : float array;
   v              : int;
   v'             : int;
   edits          : edit list array;
   
   alpha          : float;    (* dirichlet prior parameter *)
   all_alpha      : float;    (* alpha *. (# possible edit operations) *) 
   
   mutable ll     : float;
   mutable old_ll : float;

   mutable ins_count    : float array;
   mutable del_count    : float array;
   mutable sub_count    : float array array;
   mutable end_count    : float;
   mutable all_count    : float;

   beta          : float array array }
;;
let get_data fd = 
   let lines      = lines_of_file fd in
   let words      = A.make (A.length lines) "" in
   let counts     = A.make (A.length lines) 1. in
   let chars      = Hs.create 100 'a' in  
   A.switeri lines (fun i ln -> 
      match split ln with
         | n :: s :: _ ->
            counts.(i)  <- fos n;
            words.(i)   <- S.remove '-' s
         | _ -> failwith "bad text file"
   );
   A.iter (fun w -> L.iter (Hs.add chars) (S.explode w)) words;
   let chars'     = Hs.keys chars in
   let v          = A.length chars' in 
   let idx        = Ht.of_arrays chars' (A.range 0 v) in
   let words'     = A.map (fun w -> A.map (Ht.find idx) (aol $ S.explode w)) words in
   (words', counts, chars', idx, v)
;;
let init_model p_words_fd c_words_fd p_lang c_lang alpha = 
   let (p_words, p_counts, p_chars, p_idx, v)  = get_data p_words_fd in
   let (c_words, c_counts, c_chars, c_idx, v') = get_data c_words_fd in
   let edits = A.make (A.length c_words) 

let prob_edit m edit = 
   let count = match edit with
      | Ins i     -> m.ins_count.(i)
      | Del i     -> m.del_count.(i)
      | Sub (i,j) -> m.sub_count.(i).(j)
      | End       -> m.end_count in
   (m.alpha +. count) /. (m.all_alpha +. m.all_count)
;;
let add_edit m edit = 
   m.all_count <- m.all_count +. 1.;
   match edit with
      | Ins i     -> m.ins_count.(i)      <- m.ins_count.(i) +. 1.;
      | Del i     -> m.del_count.(i)      <- m.del_count.(i) +. 1.;
      | Sub (i,j) -> m.sub_count.(i).(j)  <- m.sub_count.(i).(j) +. 1.
      | End       -> m.end_count          <- m.end_count +. 1.
;;
let remove_edit m edit = 
   m.all_count <- m.all_count -. 1.;
   match edit with
      | Ins i     -> m.ins_count.(i)      <- m.ins_count.(i) -. 1.;
      | Del i     -> m.del_count.(i)      <- m.del_count.(i) -. 1.;
      | Sub (i,j) -> m.sub_count.(i).(j)  <- m.sub_count.(i).(j) -. 1.;
      | End       -> m.end_count          <- m.end_count -. 1.
;;
let compute_beta m i j =
   let b = m.beta in
   let n = A.length m.c_words.(i)
   and k = A.length m.p_words.(j) in
   b.(n+1).(k+1) <- prob_edit m End;
   for t = n + 1 downto 0 do
      for v = k + 1 downto 0 do
         let sub = prob_edit m (Sub (t,v))
         and del = prob_edit m (Del t) 
         and ins = prob_edit m (Ins v) in 
         if v <= n || t <= k then   b.(t).(v) <- 0.;
         if v <= n then             b.(t).(v) <- b.(t).(v) +. ins *. b.(t).(v+1);
         if t <= k then             b.(t).(v) <- b.(t).(v) +. del *. b.(t+1).(v);
         if v <= n && t <= k then   b.(t).(v) <- b.(t).(v) +. sub *. b.(t+1).(v+1) 
      done
   done
;;
let sample_edits m i j = 
   compute_beta m i j;
   let b = m.beta
   and c_word = m.c_words.(i)
   and p_word = m.p_words.(j) in 
   let n = A.length c_word
   and k = A.length p_word in
   let rec f = function 
      | t,v when (t,v) = (n,k) -> []
      | t,v when t = n ->  
         begin 
            let edit = Ins p_word.(v) in 
            add_edit m edit; 
            edit :: f (t, (v+1))
         end
      | t,v when v = k -> 
         begin
            let edit = Del c_word.(t) in
            add_edit m edit;
            edit :: f ((t+1), v)
         end
      | t,v ->
         begin
            let c = c_word.(t) in
            let p = p_word.(v) in
            let d = Gsl_randist.discrete_preproc
               [| prob_edit m (Sub (c,p)) *. b.(t+1).(v+1);
                  prob_edit m (Ins p)     *. b.(t).(v+1);
                  prob_edit m (Del c)     *. b.(t+1).(v)  |] in
            let edit, t', v' = match Gsl_randist.discrete rng d with 
               | 0 -> Sub (c,p), t+1, v+1
               | 1 -> Ins p, t, v+1
               | 2 -> Del c, t+1, v
               | _ -> failwith "impossible" in
            add_edit m edit;
            edit :: f (t', v')
         end
   in
   let edit_list = f (0, 0) in
   m.edits.(i) <- edit_list
;;
let possible_srcs m i = A.range 0 (A.length m.p_words)
let sample_src m i = 
   let srcs = possible_srcs m i in
   let d = Gsl_randist.discrete_preproc 
      (A.map (fun j -> compute_beta m i j; m.beta.(0).(0)) srcs) in
   Gsl_randist.discrete rng d 
;;
let sample m i = 
   L.iter (remove_edit m) m.edits.(i);
   let j = sample_src m i in
   sample_edits m i j
;;





