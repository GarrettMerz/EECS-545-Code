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
     p_lang          : string;
     c_lang          : string;
     c_chars         : char array;
     p_chars         : char array;
     c_uni           : float array;
     p_uni           : float array;
     p_idx           : (char,int) Ht.t;
     c_idx           : (char,int) Ht.t;
     c_texts         : int array array;   (** the cipher-text words *)
     c_counts        : float array;       (** number of times each cipher word appears *)
     v               : int;               (** length of p_chars *)
     v'              : int;               (** length of c_chars *)
     mutable ll      : float;
     mutable old_ll  : float;
     
     (* EM tables *)
     mutable alphas        : float array array array array;
     mutable betas         : float array array array array;
     mutable s             : float array array; (* s.(p).(c) --  'b' in Rabiner  -- P(c | p) -- emission*)
     mutable count         : float array array; (* ""        *)
     
     (* language model *)
     tri          : float array array array; (* tri.(i).(j).(k) = P(k | i,j)  *)
  } 
 
let print_tg m = 
   let v = m.v in
   let get_char i = if i = v then '_' else m.p_chars.(i) in
   for i = 0 to v do
      for j = 0 to v do
         for k = 0 to v do
            brintf "%c %c %c        %f" (get_char i) (get_char j) (get_char k) m.tri.(i).(j).(k)
         done
      done
   done
;;

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
let write_model model fd = 
   let ch = open_out fd in
   Marshal.to_channel ch model [Marshal.No_sharing];
   close_out ch
;;
let read_model fd = 
   let ch = open_in fd in
   let model = Marshal.from_channel ch in
   close_in ch;
   model
;;
let init_model tg_file text_file p_ug_file c_ug_file p_lang c_lang = 
   print_endline tg_file;
   let lines = aol (Std.input_list (Pervasives.open_in tg_file)) in
   (* let lines = lines_of_file tg_file in *)
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
      | _ -> failwith "bad tri-gram file...1");
   let p_chars = Hs.to_array chars in
   let v = A.length p_chars in
   let words, counts = get_text_data text_file in
   (* let words = array_of_file text_file in *)
   let chars = Hs.create 100 in
   A.iter (fun w -> L.iter (Hs.add chars) (S.explode w)) words;
   let c_chars = Hs.to_array chars in
   let v' = A.length c_chars in
   let p_idx = Ht.of_arrays p_chars (A.range 0 v) in
   let c_idx = Ht.of_arrays c_chars (A.range 0 v') in
   let p_uni = A.make v 0. in
   let c_uni = A.make v' 0. in
   A.switer (aol (Std.input_list (Pervasives.open_in p_ug_file)))
      (fun ln ->
         match split ln with a :: [b] -> 
            if S.length a = 1 then 
               p_uni.(Ht.find p_idx a.[0]) <- fos b
         | _ -> failwith "bad unigram file!");
   A.switer (aol (Std.input_list (Pervasives.open_in c_ug_file)))  
      (fun ln ->
         match split ln with a :: [b] ->
            if S.length a = 1 then
               c_uni.(Ht.find c_idx a.[0]) <- fos b
         | _ -> failwith "bad unigram file!!");
   A.normalize p_uni;
   A.normalize c_uni;      
   let tri = A.make_3D (v+1) (v+1) (v+1) 0. in
   let get_char s = 
      if s = "<s>" or s = "</s>" then v
      else begin
         assert (S.length s = 1);
         Ht.find p_idx s.[0]
      end
   in
   A.switer lines (fun ln ->
      match split ln with 
      | a :: b :: [c] ->
         let i = get_char a in
         let j = get_char b in
         if i != v || j != v then
            tri.(v).(i).(j) <- fos c
      | a :: b :: c :: [d] ->
         let i = get_char a in
         let j = get_char b in
         let k = get_char c in
         if j != v then
            tri.(i).(j).(k) <- fos d
      | _ -> failwith "bad trigram file...2");
   (* let get idx = 
       if idx < v then p_chars.(idx) else if idx = v then '#' else (failwith "bloah") in *)
   A.iter_2D (fun a -> try A.normalize a with Invalid_argument _ -> ()) tri;
   (* A.iterij_2D (fun i j a -> brintf "%c %c:   %s" (get i) (get j) (A.to_str1 (sprintf "%f") a)) tri; *)
   let c_texts = A.map (fun s -> A.map (Ht.find c_idx) (aol $ S.explode s)) words in
   let s = A.make_matrix v v' 0. in
   let alpha = A.make v' 100. in
   A.iter (fun a -> Gsl_randist.dirichlet rng ~alpha:alpha ~theta:a) s;
   let alphas  = A.map (fun word -> A.make_3D (A.length word) (v+1) (v+1) 0.) c_texts in
   let betas   = A.map (fun word -> A.make_3D (A.length word) (v+1) (v+1) 0.) c_texts in
   {
      p_lang   = p_lang;
      c_lang   = c_lang;
      c_chars  = c_chars;
      p_chars  = p_chars;
      c_texts  = c_texts;
      c_counts = counts;
      p_idx    = p_idx;
      c_idx    = c_idx;
      p_uni    = p_uni;
      c_uni    = c_uni;
      old_ll   = neg_infinity;
      ll       = neg_infinity;
      v        = v;
      v'       = v';
      alphas   = alphas;
      betas    = betas;
      s        = s;
      count    = A.make_matrix v v' 0.;
      tri      = tri
   }
;;
let viterbi model = 
   let c_texts = model.c_texts in
   let v       = model.v in
   let s       = model.s in
   let tri     = model.tri in   
   let res = A.map_matrix (fun _ -> -1) c_texts in
   let probs = A.make (A.length res) 0. in
   for _' = 0 to A.length c_texts - 1 do
      let word = c_texts.(_') in
      let word' = res.(_') in
      let m = A.length word in
      let maxs = A.make_3D m (v+1) (v+1) 0. in
      let args = A.make_3D m (v+1) (v+1) (-1) in
      let c = word.(0) in
      for i = 0 to v - 1 do
         maxs.(0).(v).(i) <- tri.(v).(v).(i) *. s.(i).(c)
      done;
      (* brintf "\n\nt = 0\n%s" (matrix_to_str sof maxs.(0));
      brintf "\n%s" (matrix_to_str soi args.(0)); *)
      for t = 1 to m - 1 do
         let c = word.(t) in
         for j = 0 to v - 1 do
            for k = 0 to v - 1 do
               let f i = maxs.(t-1).(i).(j) *. tri.(i).(j).(k) in
               let mx, argmx = Maths.max_and_arg ~n:(v+1) ~f in
               maxs.(t).(j).(k) <- (mx *. s.(k).(c));
               args.(t).(j).(k) <- argmx
            done
         done;
         (* brintf "\n\nt = %d\n%s" t (matrix_to_str sof maxs.(t));
         brintf "\n%s" (matrix_to_str soi args.(t)); *)
      done;
      let prev1, prev2 = Pair.map ref (Matrix.arg (>) maxs.(m-1)) in
      probs.(_') <- maxs.(m-1).(!prev1).(!prev2);
      for t = m - 1 downto 0 do
         word'.(t) <- !prev2;
         let new_prev1 = args.(t).(!prev1).(!prev2) in
         prev2 := !prev1;
         prev1 := new_prev1
      done
   done;
   (res,probs)
;;
let out_table out t chars1 chars2 = 
   let v1 = A.length chars1 in
   let v2 = A.length chars2 in
   for i = 0 to v1 - 1 do
      fprintf out "%c\t" chars1.(i); 
      let a = A.map2 (fun a b -> (a,b)) chars2 t.(i) in
      A.fast_sort (fun x y -> -1 * (compare (snd x) (snd y))) a;
      for j = 0 to min (v2 - 1) 6 do
         fprintf out " %c,%f" (fst a.(j)) (snd a.(j))
      done;
      fprintf out "\n"
   done   
;;
let invert_emissions m = 
   A.init_matrix m.v' m.v 
      (fun i j -> 
         m.s.(j).(i) *. m.p_uni.(j) /. m.c_uni.(i))   
;;   
let out_model out m = 
   fprintf out "%s (p) ->  %s (c)\n" m.p_lang m.c_lang;
   fprintf out "===================================\n";
   out_table out m.s m.p_chars m.c_chars;
   fprintf out "\n";
   fprintf out "%s (c) ->  %s (p)\n" m.c_lang m.p_lang;
   fprintf out "===================================\n";
   out_table out (invert_emissions m) m.c_chars m.p_chars;
   fprintf out "\n";
   flush out
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
   let tri     = model.tri in
   let c_counts   = model.c_counts in
   (*>*)
   (* a. *)
   model.count    <- A.make_matrix v v' 0.;
   model.old_ll   <- model.ll;
   model.ll       <- 0.;
   for _' = 0 to A.length c_texts - 1 do
      let word = c_texts.(_') in
      let n = c_counts.(_') in
      let m = A.length word in
      let a = alphas.(_') in
      let b = betas.(_') in
      (* b. *)
      let c = word.(0) in
      for i = 0 to v - 1 do
         a.(0).(v).(i) <- tri.(v).(v).(i) *. s.(i).(c);
         (* brintf "a.(0).(%d).(%d) <- tri.(%d).(%d).(%d) [%f] *. s.(%d).(%d) [%f]" v i v v i tri.(v).(v).(i) i c s.(i).(c); *)
      done;
      (* c. *)
      for t = 1 to m - 1 do
         let c = word.(t) in
         for j = 0 to v - 1 do
            for k = 0 to v - 1 do
               a.(t).(j).(k) <- 0.;
               let i1, i2 = if t = 1 then (v,v) else (0,v-1) in
               for i = i1 to i2 do
                  a.(t).(j).(k) <- a.(t).(j).(k) +. a.(t-1).(i).(j) *. tri.(i).(j).(k)
               done;
               a.(t).(j).(k) <- a.(t).(j).(k) *. s.(k).(c);
            done
         done
      done;
      (* d. *)
      let i1 = if m = 1 then v else 0 in
      let i2 = if m = 1 then v else v - 1 in
      for i = i1 to i2 do
         for j = 0 to v - 1 do
            b.(m-1).(i).(j) <- tri.(i).(j).(v);
         done
      done;
      (* e. *)
      for t = m - 2 downto 0 do
         let c = word.(t+1) in
         let i1 = if t = 0 then v else 0 in
         let i2 = if t = 0 then v else v - 1 in
         for i = i1 to i2 do
            for j = 0 to v - 1 do
               b.(t).(i).(j) <- 0.;
               for k = 0 to v - 1 do
                  b.(t).(i).(j) <- b.(t).(i).(j) +. b.(t+1).(j).(k) *. tri.(i).(j).(k) *. s.(k).(c)
               done;
            done
         done
      done;
      let z = ref 0. in
      for i = 0 to v - 1 do
         z := !z +. a.(0).(v).(i) *. b.(0).(v).(i)
      done;
      let z = !z in
      assert (z > 0.);
      model.ll <- model.ll +. n *. log z;
      for t = 0 to m - 1 do
         let c = word.(t) in
         for j = 0 to v - 1 do
            let a' = Maths.sum ~f:(fun i -> a.(t).(i).(j)) ~n:(v+1) in
            let b' = Maths.sum ~f:(fun i -> b.(t).(i).(j)) ~n:(v+1) in
            count.(j).(c) <- count.(j).(c) +. n *. a' *. b' /. z
         done

      done
   done;
   (* g. *)
   A.iter A.normalize count;
   model.s <- count
;;

let main () = 
   let r = G.create 500 in
   let eps = G.create 0.1 in
   let tg_dir = "/afs/csail.mit.edu/u/b/bsnyder/research/LM/char/" in
   let txt_file = G.create "/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/ugaritic.data" in
   let tag = G.empty "tag" in
   let out_file = G.empty "output file" in
   let usage_msg = "ugaritic_EM_tg [options] comparison_language\n" in
   let p_lang = G.empty "reference language" in
   let set_seed n = Gsl_rng.set rng (Nativeint.of_int n) in
   let speclist = Arg.align
      [  
         ("-tag", Arg.String (G.set tag), " <tag>");
         ("-r", Arg.Int (G.set r), " <int> max number of EM rounds (default=500)");
         ("-eps", Arg.Float (G.set eps), " <float> stopping criterion (default 0.1)"); 
         ("-seed", Arg.Int set_seed, " <int> random seed");
         ("-txt", Arg.String (G.set txt_file), " <file> text (data) file");
         (* ("-no_end", Arg.Set no_end, " don't use termination probabilities"); *)
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
            | 1 -> G.set p_lang s
            | _ -> arg_error "** too many args")
      usage_msg;
   if G.notdef p_lang then 
      arg_error "** must specify comparison language: hebrew, arabic, aramaic";   
   (* let c_lang = Filename.basename (G.get txt_file) in *)
   let c_lang = "ugaritic" in
   let tg_file = tg_dir ^ (G.get p_lang) ^ ".lm3" in
   let p_ug_file = tg_dir ^ (G.get p_lang) ^ ".lm1" in
   let c_ug_file = tg_dir ^ c_lang ^ ".lm1" in
   let tag = if G.isdef tag then ":" ^ G.get tag else "" in
   if G.notdef out_file then
      G.set out_file ("tg:" ^ c_lang ^ "-" ^ (G.get p_lang) ^ tag ^ ".log"); 
   let model_out_file = "tg:" ^ c_lang ^ "-" ^ (G.get p_lang) ^ tag ^ ".model" in
   let viterbi_out = "tg:" ^ c_lang ^ "-" ^ (G.get p_lang) ^ tag ^ ".vitberi" in
   let viterbi_probs_out = "viterbi_probs" in
   let m = init_model tg_file (G.get txt_file) p_ug_file c_ug_file (G.get p_lang) c_lang in
   (* print_tg m; *)
   let out_ch = open_out (G.get out_file) in
   out_model out_ch m;
   out_model stderr m;
   let rec learn i = 
      iterate m;
      let diff = m.ll -. m.old_ll in
      fprintf out_ch "%f (%f)\n" m.ll diff;
      fprintf stderr "%f (%f)\n" m.ll diff;
      fprintf out_ch "Round %d\n" (i+1);
      fprintf stderr "Round %d\n" (i+1);
      out_model out_ch m;
      out_model stderr m;
      if i < G.get r && diff > G.get eps then
         learn (i+1)
   in 
   learn 0;
   close_out out_ch;
   m.alphas <- [||]; 
   m.betas <- [||];
   write_model m model_out_file;
   let vtb_ch = open_out viterbi_out in
   let get_char i = if i = m.v then '_' else m.p_chars.(i) in
   let f a = output_line vtb_ch (S.implode (loa $ A.map get_char a)) in
   let res,probs = viterbi m in
   A.iter f res;
   let probs_ch = open_out viterbi_probs_out in
   let g a = output_line probs_ch (sof a) in
   A.iter g probs;
   close_out vtb_ch;
   close_out probs_ch     
;;

main ()
      
   
