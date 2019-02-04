(* #! /usr/bin/env ocamlscript
Ocaml.packs := ["ben";"gsl";"hhl"];
Ocaml.ocamlflags := ["-g"]
-- *)
open Printf
open Util
open Std

let rng = Gsl_rng.make (Gsl_rng.default ());;
Random.self_init ();;
let seed = Random.nativeint (Nativeint.of_int max_int);;
Gsl_rng.set rng seed;;


type model = {
     c_chars      : char array;
     p_chars      : char array;
     p_idx        : (char,int) Ht.t;
     c_idx        : (char,int) Ht.t;
     c_texts      : int array array;
     v            : int;   (** length of p_chars *)
     v'           : int;   (** length of c_chars *)
     mutable ll   : float;
     mutable old_ll  : float;
     
     (* EM tables *)
     alphas       : float array array array;
     betas        : float array array array;
     mutable s    : float array array; (* s.(p).(c) --  'b' in Rabiner *)
     mutable count: float array array; (* ""        *)
     
     (* language model *)
     bi           : float array array; (* bi.(i).(j) = P(j | i)  -- 'a' in Rabiner *)
     start        : float array;
     stop         : float array
  } 
let get_text_data fd = 
   (* let lines = lines_of_file fd in *)
   let lines = aol (Std.input_list (Pervasives.open_in fd)) in
   let words = A.make (A.length lines) "" in
   let counts = A.make (A.length lines) 1. in 
   A.switeri lines (fun i ln -> match split ln with
      | n :: s :: _ -> 
         counts.(i) <- fos n;
         words.(i) <- (S.remove '#' (S.remove '-' s))
      (* | [s] ->
         words.(i) <- S.remove '-' s *)
      | _ -> failwith "bad text file...1");
   (* print_endline (A.to_str1 id words); *)
   (* print_endline (A.to_str1 sof counts); *)
   (words,counts)
;;   

let init_model bg_file text_file ~no_end = 
   let lines = aol (Std.input_list (Pervasives.open_in bg_file)) in   
   let chars = Hs.create 100 in
   A.switer lines (fun ln -> 
      match split ln with
      | a :: b :: _ -> 
         if S.length a = 1 then
            Hs.add chars a.[0]
         else assert (a = "<s>" || a = "</s>");
         if S.length b = 1 then
            Hs.add chars b.[0]
         else assert (b = "<s>" || b = "</s>")
      | _ -> failwith "bad n-gram file...1");
   let p_chars = Hs.to_array chars in
   let v = A.length p_chars in
   let words, counts = get_text_data text_file in
   let chars = Hs.create 100 in
   A.iter (fun w -> L.iter (Hs.add chars) (S.explode w)) words;
   let c_chars = Hs.to_array chars in
   let v' = A.length c_chars in
   let p_idx = Ht.of_arrays p_chars (A.range 0 v) in
   let c_idx = Ht.of_arrays c_chars (A.range 0 v') in
   let bi = A.make_matrix v v 0. in
   let start = A.make v 0. in
   let stop = A.make v 0. in
   A.switer lines (fun ln ->
      match split ln with
      | a :: b :: [c] ->
         if a = "<s>" && b = "</s>" then ()
         else if a = "<s>" then begin
            assert (S.length b = 1);
            let j = Ht.find p_idx b.[0] in
            start.(j) <- fos c end 
         else if b = "</s>" then begin
            assert (S.length a = 1);
            let i = Ht.find p_idx a.[0] in
            stop.(i) <- if no_end then 0. else fos c end 
         else begin
            assert (S.length b = 1 && S.length a = 1);
            let i = Ht.find p_idx a.[0] in
            let j = Ht.find p_idx b.[0] in
            bi.(i).(j) <- fos c end
      | _ -> 
         failwith "bad bigram file...2");
   A.normalize start;
   for i = 0 to A.length bi - 1 do
      let tmp = A.init (v + 1) (fun j -> if j < v then bi.(i).(j) else stop.(i)) in
      A.normalize tmp;
      stop.(i) <- if no_end then 1. else tmp.(v);
      A.blit tmp 0 bi.(i) 0 v
   done;
   let c_texts = A.map (fun s -> A.map (Ht.find c_idx) (aol $ S.explode s)) words in
   let s = A.make_matrix v v' 0. in
   let alpha = A.make v' 100. in
   A.switer s (fun a -> 
      Gsl_randist.dirichlet rng ~alpha:alpha ~theta:a);
   let alphas  = A.smap c_texts (fun word -> A.make_matrix (A.length word) v 0.) in
   let betas   = A.smap c_texts (fun word -> A.make_matrix (A.length word) v 0.) in   
   {  
      c_chars  = c_chars;
      p_chars  = p_chars;
      c_texts  = c_texts;
      p_idx    = p_idx;
      c_idx    = c_idx;
      old_ll   = neg_infinity;
      ll       = neg_infinity;
      v        = v;
      v'       = v';
      alphas   = alphas;
      betas    = betas;
      s        = s;
      count    = A.make_matrix v v' 0.;
      bi       = bi;
      start    = start;
      stop     = stop 
   }
;;
let viterbi model = 
   let c_chars, p_chars, c_texts, v, v', alphas, betas, s, count, bi, start, stop = 
      model.c_chars, model.p_chars, model.c_texts, model.v, model.v', model.alphas,
      model.betas, model.s, model.count, model.bi, model.start, model.stop in
         
   let res = A.map_matrix (fun _ -> 0) c_texts in
   for _' = 0 to A.length c_texts - 1 do
      let word = c_texts.(_') in
      let word' = res.(_') in
      let m = A.length word in
      let maxs = A.map_matrix (fun _ -> 0.) alphas.(_') in
      let args = A.map_matrix (fun _ -> -1) alphas.(_') in
      let c = word.(0) in 
      for i = 0 to v - 1 do
         maxs.(0).(i) <- start.(i) *. s.(i).(c)
      done;
      for t = 1 to m - 1 do
         let c = word.(t) in
         for j = 0 to v - 1 do
            let f i = maxs.(t-1).(i) *. bi.(i).(j) in 
            let mx, argmx = Maths.max_and_arg ~n:v ~f in 
            maxs.(t).(j) <- (mx *. s.(j).(c));
            args.(t).(j) <- argmx
         done
      done;
      let prev = ref (Myarray.argmax maxs.(m - 1)) in
      word'.(m - 1) <- !prev;
      for t = m - 2 downto 0 do
         prev := args.(t+1).(!prev);
         word'.(t) <- !prev
      done;
   done;
   res
;;
let print_table t chars1 chars2 = 
   let v1 = A.length chars1 in
   let v2 = A.length chars2 in
   for i = 0 to v1 - 1 do
      Printf.eprintf "%c\t" chars1.(i); 
      let a = A.map2 (fun a b -> (a,b)) chars2 t.(i) in
      A.fast_sort (fun x y -> -1 * (compare (snd x) (snd y))) a;
      for j = 0 to min (v2 - 1) 6 do
         Printf.eprintf " %c,%f" (fst a.(j)) (snd a.(j))
      done;
      prerr_newline();
   done   
;;      
let print_model m = 
   let c_chars, p_chars, s = m.c_chars, m.p_chars, m.s in
   print_table s p_chars c_chars;
;;
let iterate model =  
   (*<*) 
   let c_texts = model.c_texts in
   let v       = model.v in
   let v'      = model.v' in
   let alphas  = model.alphas in
   let betas   = model.betas in
   let s       = model.s in
   let count   = model.count in
   let bi      = model.bi in
   let start   = model.start in
   let stop    = model.stop in 
   (*>*)
   (* a. *)
   model.count    <- A.make_matrix v v' 0.;
   model.old_ll   <- model.ll;
   model.ll       <- 0.;
   for _' = 0 to A.length c_texts - 1 do
      let word = c_texts.(_') in
      let m = A.length word in (* 'T' in Rabiner *)
      let a = alphas.(_') in
      let b = betas.(_') in
      (* b. *)
      let c = word.(0) in
      for i = 0 to v - 1 do
         a.(0).(i) <- start.(i) *. s.(i).(c)
      done;
      (* c. *)
      for t = 1 to m - 1 do
         let c = word.(t) in
         for j = 0 to v - 1 do
            a.(t).(j) <- 0.;
            for i = 0 to v - 1 do
               a.(t).(j) <- a.(t).(j) +. a.(t-1).(i) *. bi.(i).(j) 
            done;
            a.(t).(j) <- a.(t).(j) *. s.(j).(c)
         done
      done;
      (* d. *)
      for i = 0 to v - 1 do 
         b.(m-1).(i) <- stop.(i) 
      done;
      (* e. *)
      for t = m - 2 downto 0 do
         let c = word.(t+1) in
         for i = 0 to v - 1 do
            b.(t).(i) <- 0.;
            for j = 0 to v - 1 do
               b.(t).(i) <- b.(t).(i) +. b.(t+1).(j) *. bi.(i).(j) *. s.(j).(c)
            done
         done
      done;
      let z = ref 0. in
      for i = 0 to v - 1 do
         (* z := !z +. a.(m-1).(i)  *)
         z := !z +. a.(m-1).(i) *. b.(m-1).(i)
      done;
      let z = !z in
      model.ll <- model.ll +. log z;
      for t = 0 to m - 1 do
         let c = word.(t) in
         for i = 0 to v - 1 do
            count.(i).(c) <- count.(i).(c) +. a.(t).(i) *. b.(t).(i) /. z
         done
      done
   done;
   (* g. *)
   A.iter A.normalize count;
   model.s <- count
;;
let main () = 
   let no_end = ref false in
   let r = G.create 500 in
   let eps = G.create 0.1 in
   let bg_dir = "/afs/csail.mit.edu/u/b/bsnyder/research/LM/char/" in
   let txt_file = G.create "/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/ugaritic.data" in
   let out_file = G.empty "output file" in
   let usage_msg = "ugaritic_EM [options] comparison_language\n" in
   let lang = G.empty "reference language" in
   let set_seed n = Gsl_rng.set rng (Nativeint.of_int n) in
   let speclist = Arg.align
      [  
         ("-r", Arg.Int (G.set r), " <int> max number of EM rounds (default=500)");
         ("-eps", Arg.Float (G.set eps), " <float> stopping criterion (default 0.1)"); 
         ("-seed", Arg.Int set_seed, " <int> random seed");
         ("-txt", Arg.String (G.set txt_file), " <file> text (data) file");
         ("-no_end", Arg.Set no_end, " don't use termination probabilities");
         ("-o", Arg.String (G.set out_file), " <file> output file")
      ] in
   let arg_error str = 
      Arg.usage speclist usage_msg;
      prerr_endline str;
      exit 2 in
   let arg_pos = ref 0 in
   Arg.parse speclist
      (fun s ->
         incr arg_pos;
         match !arg_pos with
            | 1 -> G.set lang s
            | _ -> arg_error "** too many args")
      usage_msg;
   if G.notdef lang then 
      arg_error "** must specify comparison language: hebrew, arabic, aramaic";   
   let bg_file = bg_dir ^ (G.get lang) ^ ".lm2" in
   if G.notdef out_file then
      G.set out_file ("ugaritic2_bg_" ^ (G.get lang) ^ ".out"); 
   let m = init_model bg_file (G.get txt_file) ~no_end:(!no_end) in
   print_model m;
   let rec learn i = 
      iterate m;
      let diff = m.ll -. m.old_ll in
      prerr_endline (sprintf "%f (%f)" m.ll (diff));
      prerr_endline ("Round " ^ (soi $ i+1));
      print_model m;
      if i < G.get r && diff > G.get eps then
         learn (i+1)
   in 
   learn 0;
   let out_ch = open_out (G.get out_file) in
   let f a = output_line out_ch (S.implode (loa $ A.map (A.get m.p_chars) a)) in
   let a = viterbi m in
   brintf "%d" (A.length a);
   A.iter f a     
;;

main ()
      
   
