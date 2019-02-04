open Util, Batteries, Batteries.Standard, Print
module Fst = OpenFst

let _DEBUG = ref false;;
let only_plene = true;;
let one_step = false;;

let know_cogs        = false;;
let use_freq         = false;;
let use_english      = false;;
let make_all_cog     = false;;
let only_gammas      = false;;
let use_ug_lm        = true;;
let use_morph        = true;;

let start_time = iof (Unix.time ());;

(* let myseed = Some 50 *)
let myseed = None;;

let rng = Gsl_rng.make (Gsl_rng.default ());;
Random.self_init ();;
let seed = match myseed with Some n -> n | None -> Random.int 99999;;
Gsl_rng.set rng (Nativeint.of_int seed);;
Random.init seed;;
let mtrng = Fst.new_rng seed;;

let sample ?(sum=None) probs =
   assert (A.length probs > 0);
   assert (A.for_all (fun v -> v >= 0.) probs);
   let prob_sum = match sum with None -> A.sum probs | Some v -> v in
   assert (prob_sum > 0.);
   let resi = ref (-1) in
   let rn = Fst.random mtrng prob_sum in
   let accum = ref 0.0 in
   begin try
      A.iteri (fun i prb ->
         accum := !accum +. prb;
         if rn < !accum then begin
            resi := i;
            raise Done
         end) probs
   with Done -> () end;
   assert (!resi <> (-1));
   !resi
;;

let datadir = if use_english  then "/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/english/" 
                              else "/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/";;
let cogs             = ref 0
let init             = ref true
let r                = ref (-1);;

let burnin = 10;;

let params = if A.length Sys.argv = 2 then Some (Ht.of_file id id Sys.argv.(1)) else None;;
let get_param v s = match params with None -> v | Some ht -> if Ht.mem ht s then fos (Ht.find ht s) else v
let get_param_s v s = match params with None -> v | Some ht -> if Ht.mem ht s then Ht.find ht s else v

let cog_prior        = get_param 0.5 "cog_prior"
let gamma_sub_true   = get_param 1.0 "gammas_sub_true"
let gamma_sub_false  = get_param 1.0 "gammas_sub_false"

let gamma_ins_true   = get_param 50.0 "gammas_ins_true" 
let gamma_ins_false  = get_param 1.0 "gammas_ins_false"
let gamma_del_true   = get_param 1.0 "gammas_del_true"
let gamma_del_false  = get_param 1.0 "gammas_del_false"
let ins_prior        = get_param 0.0 "ins_prior"
let del_prior        = get_param 0.0 "del_prior"

let alpha_pre        = get_param 1000.0 "alpha_pre"
let alpha_stm        = get_param 1000.0 "alpha_stm"
let alpha_suf        = get_param 1000.0 "alpha_suf"

let rounds           = iof (get_param 250. "rounds")
let hb_budget        = iof (get_param 8. "hb_budget")
let ug_budget        = iof (get_param 0. "ug_budget")
let penalty          = get_param (-50.) "penalty"

let output_dir       = if A.length Sys.argv = 3 && Sys.argv.(1) = "-o" then Sys.argv.(2) else get_param_s "output/" "output_dir";;

Unix.system ("mkdir -p " ^ output_dir);;
let log_out         = open_out ~mode:[`trunc;`text;`create] (output_dir ^ "/log.txt");;
let printf fmt = ksprintf (fun s -> print_endline s; output_string log_out (s ^ "\n"); flush log_out) fmt;;
let drintf fmt = if !_DEBUG then ksprintf print_endline fmt else ifprintf true fmt;;
let print_params () =
   let s1 = [(use_english,"use_english"); (make_all_cog,"make_all_cog"); (only_gammas, "only_gammas"); (use_ug_lm,"use_ug_lm"); (use_morph,"use_morph"); (know_cogs,"know_cogs")] in
   L.iter (fun (v,s) -> printf p"%s     =  %B" s v) s1;
   let s2 = [  (cog_prior,"cog_prior"); (gamma_sub_true,"gamma_sub_true"); (ins_prior, "ins_prior"); 
               (alpha_pre,"alpha_pre"); (alpha_stm,"alpha_stm"); (alpha_suf,"alpha_suf"); (foi hb_budget,"hb_budget"); (foi ug_budget, "ug_budget"); (penalty, "penalty")] in
   L.iter (fun (v,s) -> printf p"%s    =  %f" s v) s2
in
print_params ();;

let anneal        = false;;
let temp_anneal   = ref false;;
let anneal_begin  = 1.0;;
let anneal_end    = 5.0;; 
let anneal_burn   = 5;;
let schedule =
   let n = rounds - anneal_burn in
      let d = (anneal_end -. anneal_begin) /. (foi n) in 
      A.init rounds (fun i -> if i < anneal_burn then 1. else d *. (foi (i - anneal_burn)) +. anneal_begin)
;;

type fst = Fst.fst 
type edit = Ins of int | Del of int | Sub of (int * int) | End 
type morph = int array 
type pos = string 
type mtype = Pre | Stm | Suf  
type word = {
   mutable pre   : morph;
   mutable stm   : morph;
   mutable suf   : morph;   } 
;;
type possible = {p_ug : word; p_hb : word; p_pre_ed : edit list; p_stm_ed : edit list; p_suf_ed : edit list; p_pos : string}
type entry = {
   true_word_ug         : word;
   true_words_hb        : word list;
   true_pos             : pos;
   true_cog             : bool;
   segments_ug          : word array;
   segments_record      : (word, int) Ht.t;
   count                : int;   
   mutable pre_tbl      : int Hs.t;
   mutable stm_tbl      : int Hs.t;
   mutable suf_tbl      : int Hs.t;
   mutable all_possible : possible array;
   mutable all_probs    : float array;
   mutable word_hb      : word;
   mutable word_ug      : word;
   mutable pos          : pos;
   mutable cog          : bool;
   mutable cog_prob     : float;
   all_cog_prob         : float array;
   mutable xxx          : bool;
   mutable best_hb      : word;
   mutable best_ug      : word;
   mutable best_pos     : pos;
   mutable best_cog     : bool;
   mutable best_prob    : float;
   mutable pre_ed       : edit list;
   mutable stm_ed       : edit list;
   mutable suf_ed       : edit list;
   mutable best_pre_ed       : edit list;
   mutable best_stm_ed       : edit list;
   mutable best_suf_ed       : edit list;   } 
 
;;
type mapping = int list array
type fsa_record = {pre_fsa : fst; stm_fsa : fst; suf_fsa : fst} 
type data = {
   encode_ug_chr        : char -> int;
   decode_ug_chr        : int -> char;
   encode_hb_chr        : char -> int;
   decode_hb_chr        : int -> char;
   encode_hb            : string -> morph;
   decode_hb            : morph -> string;
   encode_ug            : string -> morph;
   decode_ug            : morph -> string;
   num_hb_chr           : int;
   num_ug_chr           : int;
   poses                : pos array;
   pos_freq             : (pos, float) Ht.t;
   hb_freq              : ((morph * pos * mtype), float) Ht.t;
   fsas                 : (pos, fsa_record) Ht.t;
   mutable n            : float;
   entries              : entry array;
   mapping1             : mapping;  (* ug -> hb *)
   mapping2             : mapping;  (* hb -> ug *)
   must1                : (int Hs.t) array;  (* ug -> hb *)
   must2                : (int Hs.t) array;  (* hb -> ug *)
   mapping3             : int list; (* hb insertion characters *)
   edits                : edit list;
   edit_count           : (edit, float) Ht.t; (* only counts edits once per morph-type *)
   true_edit_count      : (edit, float) Ht.t; (* counts ALL edits, even for repeated morph-types *)
   best_edit_count      : (edit, float) Ht.t;
   mutable sub_sum      : float;
   mutable ins_sum      : float;
   mutable del_sum      : float;
   mutable true_sub_sum : float;
   mutable true_ins_sum : float;
   mutable true_del_sum : float;
   pos_count            : (pos, float) Ht.t;
   pos_affix_sum        : ((pos * mtype), float) Ht.t;  (* counts the number of words with pos having a non-empty hebrew affix *)
   pos_affix_count      : ((morph * pos * mtype), float) Ht.t; (* counts the number of words with pos that have hebrew affix morph *)
   mrf_count            : ((morph * pos * mtype), (edit list, float) Ht.t) Ht.t;
   best_mrf_count       : ((morph * pos * mtype), (edit list, float) Ht.t) Ht.t;
   best_mrf_count2      : ((morph * pos * mtype), (edit list, float) Ht.t) Ht.t;   (* purely for printing purposes *)
   tbls                 : ((morph * pos * mtype), (edit list, (int Hs.t) Da.t) Ht.t) Ht.t;
   mrf_sum              : ((morph * pos * mtype), float) Ht.t;
   gammas               : (edit, bool) Ht.t;  (* indicator variables for slab (true) vs spike (false) *) 
   mutable alphas_sub_sum   : float;  (* should rename... these are the alphas (dirichlet hyperparameters) for the base distribution(s) *)
   mutable alphas_ins_sum   : float;
   mutable alphas_del_sum   : float;
} 
;;
let fsa_cache = Ht.create 2000;;
let possible_edits_cache = Ht.create 2000;;
type seg_entry = {prob : float; fsa_rec : fsa_record; seg : word; npos : string} 

let init_edit_probs = Ht.create 200;;
let gammas_record = Ht.create 100;;
let edits_record = Ht.create 100;;

let best_edit_probs = Ht.create 200;;
let ug_lm = Ht.create 200;;

let read_ugaritic_data ug_data init_seg_file hb_letters ug_letters pos_list mapping_file =
   let int_of_hb = Ht.of_file char_of_string int_of_string (datadir ^ hb_letters) in
   let hb_of_int = Ht.reverse int_of_hb in
   let int_of_ug = Ht.of_file char_of_string int_of_string (datadir ^ ug_letters) in
   let ug_of_int = Ht.reverse int_of_ug in
   let encode_hb_chr = (fun h -> if use_english then Ht.get int_of_hb h 999 else Ht.find int_of_hb h) in
   let decode_hb_chr = (fun i -> if use_english then Ht.get hb_of_int i 'X' else Ht.find hb_of_int i) in
   let encode_ug_chr = Ht.find int_of_ug in
   let decode_ug_chr = Ht.find ug_of_int in
   let encode_hb = A.map encode_hb_chr >> aol >> S.explode in
   let decode_hb = S.implode >> loa >> A.map decode_hb_chr in
   let encode_ug = A.map encode_ug_chr >> aol >> S.explode in
   let decode_ug = S.implode >> loa >> A.map decode_ug_chr in
   let num_hb_chr    = Ht.length hb_of_int - 1 in
   let num_ug_chr    = Ht.length ug_of_int - 1 in
   let init_segs =   if !_DEBUG then A.slice ~last:100 (array_of_file (datadir ^ init_seg_file))
                     else array_of_file (datadir ^ init_seg_file) in
   let poses = A.map (fun s -> fst (S.split s " ")) (array_of_file (datadir ^ pos_list)) in
   let pos_freq = Ht.create 10 in
   A.iter (fun s -> let s1,s2 = S.split s " " in Ht.add pos_freq s1 (fos s2)) (array_of_file (datadir ^ pos_list));
   let default_pos = poses.(0) in
   let get_mapping fn =
      assert (Ht.length int_of_ug = num_ug_chr + 1);
      let res = A.make (Ht.length int_of_ug) [] in 
      let proc_ln ln = 
         if ln.[0] <> '#' then
            let ug, heb = S.split ln "\t" in
            let u = (encode_ug_chr >> char_of_string) ug in
            let v = L.map (begin fun str -> 
                  let (str1,str2) = S.split str "." in
                  let h = encode_hb_chr (char_of_string str1) in
                  let prb = fos ("0." ^ str2) in
                  let edit = Sub (u,h) in
                  Ht.add init_edit_probs edit prb;  
                  h 
               end) (S.nsplit (S.strip heb) " ") in
            res.(u) <- v
      in
      A.iter proc_ln (array_of_file fn);
      (* normalize initial edit probs *)
      for u = 1 to num_ug_chr do
         let hb_chrs = A.range 1 (num_hb_chr + 1) in 
         let sum = A.fold_left (fun accum h -> accum +. Ht.get init_edit_probs (Sub (u,h)) 0.) 0. hb_chrs  in
         assert (sum > 0.);
         A.iter (begin fun h -> 
            let ed = Sub (u,h) in
            if Ht.mem init_edit_probs ed then
               let vl = Ht.find init_edit_probs ed in
               Ht.replace init_edit_probs ed (vl /. (sum *. foi (num_ug_chr + 1)))
         end)  hb_chrs;
      done;
      res
   in
   let get_segmentations (w : int array) =
      (* assumptions: stem >= 2, pre <= 2, suf <= 2 *)   
      let num_segs = match A.length w with
         | x when x >= 6 -> 9
         | 5 -> 8
         | 4 -> 6
         | 3 -> 3
         | _ -> 1 in
      let segs = A.make num_segs {pre = [||]; stm = [||]; suf = [||]} in
      let pre = ref [||] in
      let rest = ref w in
      let do_pre = ref true in
      let i = ref 0 in
      while (!do_pre) do
         let stm = ref !rest in
         let suf = ref [||] in
         let do_suf = ref true in
         while (!do_suf) do   
            segs.(!i) <- {pre = !pre; stm = !stm; suf = !suf};
            incr i;
            if A.length !stm > 2 && A.length !suf < 2 then begin
               suf := A.init (A.length !suf + 1) (fun j -> if j = 0 then A.last !stm else !suf.(j-1));
               stm := A.slice ~last:(-1) !stm;
            end else 
               do_suf := false
         done;
         if A.length !rest > 2 && A.length !pre < 2 then begin
            pre := A.init (A.length !pre + 1) (fun j -> if j = A.length !pre then !rest.(0) else !pre.(j));
            rest := A.slice ~first:1 !rest;
         end else 
            do_pre := false
      done;
      assert (!i = num_segs);
      segs
   in
   let proc_ln i ln =
      let proc_word (encode : string -> int array) w : word =
         let morphs = S.nsplita (S.rstrip ~chars:"!" w) "-" in
         match A.length morphs with
            | 3 ->
               if not (morphs.(0).[0] = '#' && morphs.(2).[S.length morphs.(2) - 1] = '#') then begin
                  A.print id morphs;
                  failwith "prefix and suffix should be marked"
               end;
               let res = A.map (S.strip ~chars:"#" >> S.remove '(' >> S.remove ')') morphs in
               (* printf p"%s" res.(1); *)
               {pre = encode res.(0); stm = encode res.(1); suf = encode res.(2)} 
            | 2 ->
               if morphs.(0).[0] = '#' then
                  let res = A.map (S.strip ~chars:"#" >> S.remove '(' >> S.remove ')') morphs in
                  {pre = encode res.(0); stm = encode res.(1); suf = [||]}
               else if morphs.(1).[S.length morphs.(1) - 1] = '#' then
                  let res = A.map (S.strip ~chars:"#" >> S.remove '(' >> S.remove ')') morphs in
                  {pre = [||]; stm = encode res.(0); suf = encode res.(1)}
               else begin
                  A.print id morphs;
                  failwith "prefix and suffix should be marked"
               end
            | 1 ->
               let res = (S.strip ~chars:"#" >> S.remove '(' >> S.remove ')') morphs.(0) in
               {pre = [||]; stm = encode res; suf = [||]}
            | _ ->
               A.print id morphs;
               failwith "should have no greater than three morphs"
      in
      let fields = S.nsplit (S.strip ln) "\t" in
      if L.length fields > 1 then begin
         let count_str, true_word_ug_str = S.split (L.hd fields) " " in
         let filter_fun w = not (S.ends_with w "!") or not only_plene in
         let true_words_hb = ref (L.map (proc_word encode_hb) (L.filter filter_fun (L.tl fields))) in

         (* add the feminine hebrew form... *)
         if (proc_word encode_ug true_word_ug_str).suf = [|14|] 
            then if L.exists (fun w -> w.suf = [|21|]) !true_words_hb
               then if not (L.exists (fun w -> w.suf = [|11|]) !true_words_hb) then 
                  begin
                     let w = L.find (fun w -> w.suf = [|21|]) !true_words_hb in
                     let w' = {pre=w.pre; stm=w.stm; suf=[|11|]} in
                     true_words_hb := w' :: !true_words_hb
                  end;

         (* add the feminine construct hebrew form... *)
         if (proc_word encode_ug true_word_ug_str).suf = [|24|] 
            then if L.exists (fun w -> w.suf = [|11|]) !true_words_hb
               then if not (L.exists (fun w -> w.suf = [|20|]) !true_words_hb) then 
                  begin
                     let w = L.find (fun w -> w.suf = [|11|]) !true_words_hb in
                     let w' = {pre=w.pre; stm=w.stm; suf=[|20|]} in
                     true_words_hb := w' :: !true_words_hb
                  end;

         let true_word_ug = proc_word encode_ug true_word_ug_str in
         Ht.incr' ug_lm (-1); (* for End *)
         A.iter (Ht.incr' ug_lm) true_word_ug.pre;
         A.iter (Ht.incr' ug_lm) true_word_ug.stm;
         A.iter (Ht.incr' ug_lm) true_word_ug.suf;

         {  true_word_ug      = true_word_ug; 
            true_words_hb     = !true_words_hb; 
            true_pos          = default_pos;
            true_cog          = true;
            segments_ug       = get_segmentations (encode_ug (true_word_ug_str |> S.remove '-' |> S.remove '#'));
            segments_record   = Ht.create 100;
            all_possible      = [||];
            all_probs         = [||];
            word_hb           = {pre = [||]; stm = [||]; suf = [||]};
            word_ug           = proc_word encode_ug (S.strip init_segs.(i));
            pos               = default_pos;
            cog               = true;
            cog_prob          = -1.0;
            all_cog_prob      = A.make 2 0.;
            pre_tbl           = Hs.create 10;
            stm_tbl           = Hs.create 10;
            suf_tbl           = Hs.create 10;
            best_ug           = {pre = [||]; stm = [||]; suf = [||]};
            best_hb           = {pre = [||]; stm = [||]; suf = [||]};
            best_pos          = default_pos;
            best_cog          = true;
            best_prob         = -1.0;
            xxx               = false;
            count             = ios (count_str);
            pre_ed   = [];
            stm_ed   = [];
            suf_ed   = [];
            best_pre_ed = [];
            best_stm_ed = [];
            best_suf_ed = [] }

      end else (* not a cognate *)
         let count_str, true_word_ug_str = S.split (L.hd fields) " " in
         let true_word_ug = {pre = [||]; stm = encode_ug (true_word_ug_str |> S.remove '-' |> S.remove '#'); suf = [||]} in
         Ht.incr' ug_lm (-1); (* for End *)
         A.iter (Ht.incr' ug_lm) true_word_ug.pre;
         A.iter (Ht.incr' ug_lm) true_word_ug.stm;
         A.iter (Ht.incr' ug_lm) true_word_ug.suf;
         
         {  true_word_ug      = true_word_ug;
            true_words_hb     = [];
            true_pos          = default_pos;
            true_cog          = false;
            segments_ug       = get_segmentations (encode_ug (true_word_ug_str |> S.remove '-' |> S.remove '#'));
            segments_record   = Ht.create 100;
            all_possible      = [||];
            all_probs         = [||];
            word_hb           = {pre = [||]; stm = [||]; suf = [||]};
            word_ug           = proc_word encode_ug (S.strip init_segs.(i));
            pos               = default_pos;
            cog               = true;
            cog_prob          = -1.0;
            all_cog_prob      = A.make 2 0.;
            pre_tbl           = Hs.create 10;
            stm_tbl           = Hs.create 10;
            suf_tbl           = Hs.create 10;
            best_ug           = {pre = [||]; stm = [||]; suf = [||]};
            best_hb           = {pre = [||]; stm = [||]; suf = [||]};
            best_pos          = default_pos;
            best_cog          = false;
            best_prob         = -1.0;
            xxx               = false;
            count             = ios (count_str);
            pre_ed   = [];
            stm_ed   = [];
            suf_ed   = [];
            best_pre_ed = [];
            best_stm_ed = [];
            best_suf_ed = [] }
   in
   let data          =  if !_DEBUG then A.slice ~last:100 (array_of_file (datadir ^ ug_data)) 
                        else array_of_file (datadir ^ ug_data) in
   assert (A.length data = A.length init_segs);
   
   let entries       =  if !_DEBUG then  A.mapi proc_ln (A.slice ~last:100 (array_of_file (datadir ^ ug_data)))
                        else A.mapi proc_ln (array_of_file (datadir ^ ug_data)) in
   
   let fsas = Ht.init poses (fun pos' -> 
      let pos = if know_cogs then pos' ^ "@" else pos' in
      {  pre_fsa  = Fst.sort_arcs (Fst.minimize (Fst.determinize (Fst.read_from_file (datadir ^"pre_" ^ pos ^ ".fsa_bin"))));
         stm_fsa  = Fst.sort_arcs (Fst.minimize (Fst.determinize (Fst.read_from_file (datadir ^"stm_" ^ pos ^ ".fsa_bin"))));  
         suf_fsa  = Fst.sort_arcs (Fst.minimize (Fst.determinize (Fst.read_from_file (datadir ^"suf_" ^ pos ^ ".fsa_bin")))); }) in
   
   let hb_freq = Ht.create 5000 in
   let keys = [? A : (mtype, pos) | mtype <- A : [|Pre; Stm; Suf|]; pos <- A : poses] in
   
   let counts = A.map (fun (mtype,pos') ->
      let pos = if know_cogs then pos' ^ "@" else pos' in
      array_of_file (datadir ^ (match mtype with Pre -> "pre" | Stm -> "stm" | Suf -> "suf") ^ "_" ^ pos ^ ".list") |>
      A.map (fun s -> let (mrf, c) = S.split s " " in (encode_hb mrf, fos c))
   ) keys in
   
   let stm_sum = ref 0. in
   A.iter2 (fun (mtype, pos) a -> if mtype = Stm then A.iter (fun (_,c) -> stm_sum := !stm_sum +. c) a) keys counts;
   
   A.iter2 (fun (mtype, pos) a -> 
      let sum = if mtype = Stm then !stm_sum else A.fold_left (fun acc (_,c) -> acc +. c) 0. a in 
      A.iter (fun (mrf,c) ->
         Ht.add hb_freq (mrf, pos, mtype) (c /. sum)
      ) a
   ) keys counts;
                               
   let mapping1      = get_mapping (datadir ^ mapping_file) in
   let mapping2      = A.make (num_hb_chr + 1) [] in
   foreach (1 -- num_ug_chr) (fun u -> L.iter (fun h -> mapping2.(h) <- u :: mapping2.(h)) mapping1.(u)); 
   let mapping3_  =  loa (A.filter (fun s -> s.[0] <> '#') (array_of_file (datadir ^ "mapping3")) |> 
                     A.map (fun s -> let s1,s2 = S.split s " " in (encode_hb_chr s1.[0], fos s2)) |>
                     A.slice ~last:2) in

   let ins_sum    = L.fold_left (fun acc p -> acc +. snd p) 0. mapping3_ in
   let mapping3   = L.map (fun (h,f) -> Ht.add init_edit_probs (Ins h) (f /. ins_sum); h) mapping3_ in   
   
   Ht.add init_edit_probs End (1. /. foi (num_ug_chr + 1)); 

   let edits   = [?L : (Sub (u,h)) | u <- 1 -- num_ug_chr; h <- 1 -- num_hb_chr; L.mem h mapping1.(u)] @
                 (* [L : ? (Del u) | u <- 1 -- num_ug_chr] @ *)
                 [?L : (Ins h) | h <- L : mapping3] @ [End] in
   
   {  encode_ug_chr  = encode_ug_chr;
      decode_ug_chr  = decode_ug_chr;
      encode_hb_chr  = encode_hb_chr;
      decode_hb_chr  = decode_hb_chr;  
      encode_ug      = encode_ug;
      decode_ug      = decode_ug;
      encode_hb      = encode_hb;
      decode_hb      = decode_hb;
      entries        = entries;
      n              = foi (A.length entries);
      fsas           = fsas;
      poses          = poses;
      pos_freq       = pos_freq;
      hb_freq        = hb_freq;
      mapping1       = mapping1;
      mapping2       = mapping2;
      mapping3       = mapping3;
      num_hb_chr     = num_hb_chr;
      num_ug_chr     = num_ug_chr;
      edit_count     = Ht.create 5000;
      best_edit_count= Ht.create 5000;
      true_edit_count= Ht.create 5000;
      sub_sum        = 0.;
      ins_sum        = 0.;
      del_sum        = 0.;
      true_sub_sum   = 0.;
      true_ins_sum   = 0.;
      true_del_sum   = 0.;
      mrf_count      = Ht.create 5000;
      best_mrf_count = Ht.create 5000;
      best_mrf_count2 = Ht.create 5000;
      tbls           = Ht.create 5000;
      pos_count      = Ht.create 10;
      pos_affix_sum  = Ht.create 20;
      pos_affix_count= Ht.create 500;
      mrf_sum        = Ht.create 5000; 
      edits          = edits;
      gammas         = Ht.create 1000;
      must1          = A.init (num_ug_chr + 1) (fun _ -> Hs.create 20);
      must2          = A.init (num_hb_chr + 1) (fun _ -> Hs.create 20);
      alphas_sub_sum = 0.;
      alphas_ins_sum = 0.;
      alphas_del_sum = 0.;
   }
;;
let d = read_ugaritic_data "ugaritic.data" "init_morfseg" "hb_letters" "ug_letters" (if use_morph then "poses.list" else "poses_nomorph.list") "mapping-updated";;

let char_index = A.init (d.num_ug_chr + 1) (*<*)
   (fun u ->
      A.map_filteri 
         (fun i e -> if A.mem u e.word_ug.pre || A.mem u e.word_ug.stm || A.mem u e.word_ug.suf then Some i else None) 
         d.entries
(*>*)   );; 
let char_logsum = log (Ht.sum' ug_lm) in A.iter (fun (u,n) -> Ht.replace ug_lm u (log n -. char_logsum)) (Ht.to_array ug_lm);;

let get_noncog_logprob ug =
   if use_ug_lm then (
      let morph_prob mrf = A.fold_left (fun acc u -> acc +. Ht.find ug_lm u) 0. mrf in
      morph_prob ug.pre +. morph_prob ug.stm +. morph_prob ug.suf +. Ht.find ug_lm (-1))
   else (
      let len = A.length ug.pre + A.length ug.stm + A.length ug.suf + 1 in
      foi len *. (log 1. -. log 31.))
;;
   
let get_morph_tables morph mtype pos =
   let tbls = d.tbls in 
   let key  = (morph, pos, mtype) in
   if not (Ht.mem tbls key) then Ht.add tbls key (Ht.create 200); 
   Ht.find tbls key
;;
let get_morph_counts morph mtype pos = 
   let mrf_count = d.mrf_count in
   let key = (morph, pos, mtype) in
   if not (Ht.mem mrf_count key) then Ht.add mrf_count key (Ht.create 200); 
   Ht.find mrf_count key
;;
let get_best_morph_counts morph mtype pos = 
   let mrf_count = d.best_mrf_count in
   let key = (morph, pos, mtype) in
   if not (Ht.mem mrf_count key) then Ht.add mrf_count key (Ht.create 200); 
   Ht.find mrf_count key
;;      
let rec edit_dist (s1 : string) (s2 : string) =
   (** computes the edit distance (Levenshtein) between two strings *)
  if s1 = "" then S.length s2 else 
    if s2 = "" then S.length s1 else
      let sub_cost = if s1.[0] = s2.[0] then 0 else 1 in
      let sub1, sub2 = S.slice ~first:1 s1, S.slice ~first:1 s2 in
      min (sub_cost + edit_dist sub1 sub2)
		  (min (1 + edit_dist sub1 s2) (1 + edit_dist s1 sub2))
;;
let string_of_edit = function 
   | End -> "End"
   | Ins x -> "Ins " ^ soc (d.decode_hb_chr x)
   | Del x -> "Del " ^ soc (d.decode_ug_chr x)
   | Sub (x,y) -> "Sub " ^ soc (d.decode_ug_chr x) ^ " " ^ soc (d.decode_hb_chr y)
;;
let edit_print out e = S.print out (string_of_edit e);;
let edit_printer paren = edit_print;;
let morph_to_string decode morf = 
   begin try
      let pre = decode morf.pre in
      let stm = decode morf.stm in
      let suf = decode morf.suf in
      if pre <> "" && suf <> "" then
         pre ^ "-" ^ stm ^ "-" ^ suf
      else if pre <> "" && suf = "" then
         pre ^ "-" ^ stm
      else if pre = "" && suf <> "" then
         stm ^ "-" ^ suf
      else stm
   with e -> Array.print (int_printer false) stdout morf.pre; 
             Array.print (int_printer false) stdout morf.stm;
             Array.print (int_printer false) stdout morf.suf;
             raise e
   end
;;
let entry_to_string entry =
   let noncog_prob = exp (get_noncog_logprob entry.word_ug) in
   let cog_prob = entry.cog_prob /. noncog_prob in
   let true_hb_str = if entry.true_cog 
               then S.concat " " (L.map (morph_to_string d.decode_hb) entry.true_words_hb)
               else "XXX" in
   let pos = if entry.cog then entry.pos else "*" in
   let ug_str = morph_to_string d.decode_ug entry.word_ug in
   let hb_str = if entry.xxx then "xxx" else morph_to_string d.decode_hb entry.word_hb in
   ug_str ^ " (" ^ pos ^ ")\t" ^ hb_str ^ "\t" ^ true_hb_str ^ "\t" ^ (sof cog_prob)   
;;
let entry_to_string_best entry =
   let cog_prob = entry.all_cog_prob.(0) /. entry.all_cog_prob.(1) in
   let true_hb_str = if entry.true_cog 
               then S.concat " " (L.map (morph_to_string d.decode_hb) entry.true_words_hb)
               else "XXX" in
   let pos = if entry.xxx then "xxx" 
      else if entry.best_pos = "-" then "-" 
      else if entry.best_cog then entry.best_pos else "*" in
   let ug_str = morph_to_string d.decode_ug entry.best_ug in
   (* let hb_str = if entry.xxx then "xxx" else morph_to_string d.decode_hb entry.best_hb in *)
   let hb_str = morph_to_string d.decode_hb entry.best_hb in
   ug_str ^ " (" ^ pos ^ ")\t" ^ hb_str ^ "\t" ^ true_hb_str ^ "\t" ^ (sof cog_prob)   
;;
let printer_entry k x = k (fun oc -> IO.nwrite oc (entry_to_string x));;
let entry_print out entry = IO.nwrite out (entry_to_string entry);;
let entry_print_best out entry = IO.nwrite out (entry_to_string_best entry);;
let ug_from_edits edit_list = 
   aol $ L.map_filter (function Del u -> Some u | Sub (u,h) -> Some u | _ -> None) edit_list
;;
let hb_from_edits edit_list = 
   aol $ L.map_filter (function Ins h -> Some h | Sub (u,h) -> Some h | _ -> None) edit_list
;;
let get_alpha ed = 
   match ed with
   | Sub _ -> if Ht.find d.gammas ed then gamma_sub_true else gamma_sub_false
   | Ins _ -> if Ht.find d.gammas ed then gamma_ins_true else gamma_ins_false
   | Del _ -> if Ht.find d.gammas ed then gamma_del_true else gamma_del_false
   | End   -> 1.0
;;
let edit_to_int = function
   | End -> 0;
   | Ins hb -> hb;
   | Del ug -> d.num_hb_chr + ug
   | Sub (ug,hb) -> 
      d.num_hb_chr + d.num_ug_chr + ((d.num_hb_chr + 1) * ug + hb)
;;
let int_to_edit = function
   | 0 -> End
   | n when n <= d.num_hb_chr -> Ins n
   | n when n <= d.num_hb_chr + d.num_ug_chr -> Del (n - d.num_hb_chr)
   | n -> 
      let n' = n - d.num_hb_chr - d.num_ug_chr in
      let ug = n' / (d.num_hb_chr + 1) in
      let hb = n' mod (d.num_hb_chr + 1) in
      Sub (ug,hb)
;;
let edit_prob ?(true_count=false) edit =
   (* take note: we may do annealing INSIDE this function *)
   if !init then begin
      (* printf p"finding init prob for edit = %{edit}" edit; *)
      Ht.find init_edit_probs edit
   end
   else begin
      let alpha = get_alpha edit in
      let edit_count = Ht.count' (if true_count then d.true_edit_count else d.edit_count) edit in
      let edit_sum = if true_count then 
         match edit with Sub _ | End -> d.true_sub_sum | Ins _ -> d.true_ins_sum | Del _ -> d.true_del_sum
      else
         match edit with Sub _ | End -> d.sub_sum | Ins _ -> d.ins_sum | Del _ -> d.del_sum 
      in
      let alphas_sum = match edit with Sub _ | End -> d.alphas_sub_sum | Ins _ -> d.alphas_ins_sum | Del _ -> d.alphas_del_sum in
      assert (edit_count >= 0.0);
      if !temp_anneal then
         ((alpha +. edit_count) /. (alphas_sum +. edit_sum)) ** schedule.(!r)
      else
         ((alpha +. edit_count) /. (alphas_sum +. edit_sum))
   end
;;
let edit_prob_c n = 
   let edit = int_to_edit n in
   -.log (edit_prob edit)
;; 
let asc_fact par count =
   let fact = ref 0.0 in
   for i = 0 to iof count - 1 do
      fact := !fact +. log (par +. foi i)
   done;
   !fact
;;
let adjust probs = 
   let mx = max_item probs in
   let k = 10. /. mx in
   A.update (fun v -> v *. k) probs
;;
let logs_to_probs a = 
   (* printf p"a = %{float array}" a; *)
   let min = ref infinity in
   let mx = ref neg_infinity in
   for i = 0 to A.length a - 1 do
      let v = a.(i) in
      if v <> neg_infinity && v < !min then
         min := v;
      if v > !mx then mx := v;
   done;
   assert (!min <> infinity);
   assert (!mx <> neg_infinity && !mx <> infinity);
   
   (* printf p" min = %f  max = %f" !min !mx; *)
   let c = 20. -. !mx in
   (* let c = max 0. (-300. -. !min) in *)
   (* printf p"c = %f, min = %f, a = %{float array}" c !min a; *)
   for i = 0 to A.length a - 1 do
      a.(i) <- exp (a.(i) +. c)
   done
;;

let construct_fsa_only_sub ug_morph =
   (* only allows substitutions (no insertions or deletions) *)
   let fsa, start_state = Fst.new_fst (mtrng) in
   let swap_states = ref [start_state] in
   for i = 0 to A.length ug_morph - 1 do
      let c          = ug_morph.(i) in
      let labels     = d.mapping1.(c) in
      let new_swap_states = L.map (fun label -> Fst.add_state fsa (edit_to_int (Sub (c,label)))) labels in
      L.iter2 (begin fun state label ->
         let swap_prob = -.log (edit_prob (Sub (c,label))) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label swap_prob) !swap_states;
      end) new_swap_states labels;
      swap_states := new_swap_states;
   done;
   let end_prob = -. log (edit_prob End) in
   L.iter (fun swap_state -> Fst.set_final fsa swap_state end_prob) !swap_states;
   fsa
;;
let construct_fsa_one_insert ug_morph = 
   (* only alllows one insertion *)
   let fsa, start_state = Fst.new_fst (mtrng) in
   
   let ins_labels = d.mapping3 in
   let ins_states = ref (L.map (fun label -> Fst.add_state fsa (edit_to_int (Ins label))) ins_labels) in
   L.iter2 (begin fun state label ->
      let ins_prob = -. log (edit_prob (Ins label)) in
      Fst.add_arc fsa start_state state label ins_prob
   end) !ins_states ins_labels;

   let swap_states1 = ref [start_state] in   (* swap states for which no insertion has yet occurred *)
   let swap_states2 = ref [] in              (* swap states for which the insertion has already been used *)
   for i = 0 to A.length ug_morph - 1 do
      let c          = ug_morph.(i) in
      let labels     = d.mapping1.(c) in
      let new_swap_states1 = L.map (fun label -> Fst.add_state fsa (edit_to_int (Sub (c,label)))) labels in
      let new_swap_states2 = L.map (fun label -> Fst.add_state fsa (edit_to_int (Sub (c,label)))) labels in

      (* create edges from previous swap states (with no insertions yet) to new swap states (with no insertions yet *)
      L.iter2 (begin fun state label ->
         let swap_prob = -.log (edit_prob (Sub (c,label))) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label swap_prob) !swap_states1;
      end) new_swap_states1 labels;

      (* create edges from previous swap states (with the insertions used) as well as insertion states,  
         to new swap states (with the insertion used) *)
      L.iter2 (begin fun state label ->
         let swap_prob = -.log (edit_prob (Sub (c,label))) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label swap_prob) !swap_states2;
         L.iter (fun ins_state -> Fst.add_arc fsa ins_state state label swap_prob) !ins_states 
      end) new_swap_states2 labels;
      
      (* create edges from new swap states (with no insertion yet) to new insertion states *)
      let new_ins_states = L.map (fun label -> Fst.add_state fsa (edit_to_int (Ins label))) ins_labels in
      L.iter2 (begin fun state label ->
         let ins_prob = -.log (edit_prob (Ins label)) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label ins_prob) new_swap_states1
      end) new_ins_states ins_labels;
      
      swap_states1 := new_swap_states1;
      swap_states2 := new_swap_states2;
      ins_states := new_ins_states
   done;
   let ins_end_prob  = -.log (edit_prob End *. ins_prior /. foi (A.length ug_morph + 1)) in
   let end_prob      = -.log (edit_prob End *. (1. -. ins_prior)) in
   L.iter (fun state -> Fst.set_final fsa state end_prob) !swap_states1;
   L.iter (fun state -> Fst.set_final fsa state ins_end_prob) !swap_states2;
   if A.length ug_morph > 0 then
      L.iter (fun state -> Fst.set_final fsa state ins_end_prob) !ins_states;
   fsa
;;
let construct_fsa ug_morph =
   let fsa, start_state = Fst.new_fst (mtrng) in
   let eps_label = 0 in
   let ins_labels = L.range 1 (d.num_hb_chr + 1) in
   let ins_states = L.map (fun label -> Fst.add_state fsa (edit_to_int (Ins label))) ins_labels in
   L.iter2 (fun state label -> 
      let ins_prob = -. log (edit_prob (Ins label)) in
      Fst.add_arc fsa start_state state label ins_prob)
      ins_states ins_labels;
   let swap_states = ref [start_state] in
   let spec_states = ref ins_states in
   for i = 0 to A.length ug_morph - 1 do
      let c          = ug_morph.(i) in
      let labels     = d.mapping1.(c) in
      let del_state  = Fst.add_state fsa (edit_to_int (Del c)) in
      let del_prob   = -.log (edit_prob (Del c)) in
      L.iter (fun swap_state -> Fst.add_arc fsa swap_state del_state eps_label del_prob) !swap_states;
      let new_swap_states = L.map (fun label -> Fst.add_state fsa (edit_to_int (Sub (c,label)))) labels in
      L.iter2 (fun state label ->
         let swap_prob = -.log (edit_prob (Sub (c,label))) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label swap_prob) !swap_states;
         L.iter (fun spec_state -> Fst.add_arc fsa spec_state state label swap_prob) !spec_states)
         new_swap_states labels;
      let new_ins_states = L.map (fun label -> Fst.add_state fsa (edit_to_int (Ins label))) ins_labels in
      L.iter2 (fun state label ->
         let ins_prob = -. log (edit_prob (Ins label)) in
         L.iter (fun swap_state -> Fst.add_arc fsa swap_state state label ins_prob) new_swap_states)
         new_ins_states ins_labels;
      swap_states := new_swap_states;
      spec_states := del_state :: new_ins_states
   done;
   let end_prob = -. log (edit_prob End) in
   L.iter (fun swap_state -> Fst.set_final fsa swap_state end_prob) !swap_states;
   L.iter (fun spec_state -> Fst.set_final fsa spec_state end_prob) !spec_states;
   fsa
;;
let get_fsa ug_morph pos mtype =
   try
      let fsa = Ht.find fsa_cache (ug_morph,pos,mtype) in
      Fst.reweight_fsa fsa;
      fsa
   with Not_found -> 
      let fsa_ = construct_fsa_one_insert ug_morph in
      let fsa = match mtype with
         | Pre -> Fst.intersect fsa_ (Ht.find d.fsas pos).pre_fsa
         | Stm -> Fst.intersect fsa_ (Ht.find d.fsas pos).stm_fsa
         | Suf -> Fst.intersect fsa_ (Ht.find d.fsas pos).suf_fsa in
      Ht.add fsa_cache (ug_morph,pos,mtype) fsa; 
      fsa
;;

let gammas_prior () = 
   (* returns log-prob ignoring the normalization constant *)
   let res = ref 0. in
   let del_count = ref 0 in
   let ins_count = ref 0 in
   let subug_counts = A.make d.num_ug_chr 0 in
   let subhb_counts = A.make d.num_hb_chr 0 in
   for u = 1 to d.num_ug_chr do
      L.iter (fun h ->
         if Ht.get d.gammas (Sub (u,h)) false then begin
            subug_counts.(u-1) <- subug_counts.(u-1) + 1;
            subhb_counts.(h-1) <- subhb_counts.(h-1) + 1
         end) d.mapping1.(u)
   done;      
   (* for u = 1 to d.num_ug_chr do   (* for deletions *)
      if Ht.get d.gammas (Del u) false then del_count := !del_count + 1
   done;
   for h = 1 to d.num_hb_chr do  (* for insertions *)
      if Ht.get d.gammas (Ins h) false then ins_count := !ins_count + 1
   done; *)
   let h_fertility = A.init (max 4 (max_item subhb_counts + 1)) (fun n -> A.count (eq n) subhb_counts) in
   let u_fertility = A.init (max 4 (max_item subug_counts + 1)) (fun n -> A.count (eq n) subug_counts) in
   
   (* drintf p"";
   drintf p"insert count : %d" !ins_count;
   drintf p"delete count : %d" !del_count; *)
   (* printf p"hb counts : %{int array}" subhb_counts; *)
   (* printf p"ug sub fertilities : %{int array}" u_fertility;
   printf p"hb sub fertilities : %{int array}" h_fertility; *)
   
   let num_zeros = h_fertility.(0) + u_fertility.(0) in
   if use_english || !_DEBUG || not use_morph || know_cogs then 
      res := !res -. 5000. *. foi num_zeros
   else if num_zeros > 0 then
      res := !res +. neg_infinity;

   let num_big = ref (max 0 (h_fertility.(3) - 1) + u_fertility.(3)) in
   for i = 4 to A.length h_fertility - 1 do
      num_big := !num_big + h_fertility.(i)
   done;
   for i = 4 to A.length u_fertility - 1 do
      num_big := !num_big + u_fertility.(i)
   done;
   if use_english || !_DEBUG || not use_morph || know_cogs then 
      res := !res -. 5000. *. foi !num_big
   else if !num_big > 0 then
      res := !res +. neg_infinity;

   let h_overbudget = foi (max 0 (h_fertility.(2) - hb_budget)) in
   let u_overbudget = foi (max 0 (u_fertility.(2) - ug_budget)) in
   res := !res +. penalty *. (h_overbudget +. u_overbudget);
   (* printf p"(3) res = %f" !res;     *)
   (* res := !res +. (if !del_count < A.length del_lp then del_lp.(!del_count) else neg_infinity);
   res := !res +. (if !ins_count < A.length ins_lp then ins_lp.(!ins_count) else neg_infinity);
   foreach (A.enum subug_counts) (fun c -> 
      res := !res +. (if c < A.length subug_lp then subug_lp.(c) else neg_infinity));
   foreach (A.enum subhb_counts) (fun c ->
      res := !res +. (if c < A.length subhb_lp then subhb_lp.(c) else neg_infinity)); *)
   assert (classify_float !res != FP_nan);
   !res
;;
let gammas_ll () =
   let res = ref 0. in
   for u = 1 to d.num_ug_chr do
      L.iter (begin fun h ->
         let sub = Sub (u,h) in
         let count = Ht.count' d.edit_count sub in
         let alpha = get_alpha sub in
         (* if h == 21 then
            printf p"%{edit} = %f    %f (%{bool}) / %f" sub count alpha (Ht.find d.gammas sub) d.alphas_sub_sum;  *)
         res := !res +. asc_fact alpha count
      end) d.mapping1.(u)
   done;
   let alphas_sub_sum = ref 1. in (* for End *)
   let sub_sum = ref (Ht.count' d.edit_count End) in
   for u = 1 to d.num_ug_chr do
      L.iter (fun h -> 
         let sub = Sub (u,h) in
         let count = Ht.count' d.edit_count sub in
         sub_sum := !sub_sum +. count;
         alphas_sub_sum := !alphas_sub_sum +. (if Ht.find d.gammas sub then gamma_sub_true else gamma_sub_false)
      ) d.mapping1.(u)
   done;
   (* printf p"1: mine = %d, theirs = %d" (iof !alphas_sub_sum) (iof d.alphas_sub_sum);
   assert (!alphas_sub_sum = d.alphas_sub_sum);
   printf p"2: mine = %d, theirs = %d" (iof !sub_sum) (iof d.sub_sum);
   assert (!sub_sum = d.sub_sum); *)
   !res -. asc_fact d.alphas_sub_sum d.sub_sum;
;;
let check_edit_counts i = 
   let alphas_sub_sum = ref 1. in (* for End *)
   let sub_sum = ref (Ht.count' d.edit_count End) in
   for u = 1 to d.num_ug_chr do
      for h = 1 to d.num_hb_chr do
         let sub = Sub (u,h) in
         let count = Ht.count' d.edit_count sub in
         sub_sum := !sub_sum +. count;
         if L.mem h d.mapping1.(u) then 
            alphas_sub_sum := !alphas_sub_sum +. (if Ht.get d.gammas sub false then gamma_sub_true else gamma_sub_false)
      done
   done;
   if not !init then begin
      (* printf p"1: mine = %d, theirs = %d (%d)" (iof !alphas_sub_sum) (iof d.alphas_sub_sum) i; *)
      assert (!alphas_sub_sum = d.alphas_sub_sum);
      (* printf p"2: mine = %d, theirs = %d (%d)" (iof !sub_sum) (iof d.sub_sum) i; *)
      assert (!sub_sum = d.sub_sum);
   end
;;   
  
let add_sub ed = 
   match ed with
   | Sub _ -> 
      if not (Ht.get d.gammas ed false) then begin
         d.alphas_sub_sum <- d.alphas_sub_sum -. gamma_sub_false +. gamma_sub_true;
         Ht.replace d.gammas ed true;
      end
   | _ -> assert false;
;;
let rem_sub ed =
   match ed with
   | Sub _ -> 
         if (Ht.find d.gammas ed) then begin
            d.alphas_sub_sum <- d.alphas_sub_sum -. gamma_sub_true +. gamma_sub_false;
            Ht.replace d.gammas ed false
         end
   | _ -> assert false;
;;
let gammas_possible poss most =
   match most with
   | 2 -> (* distribute 1 - 2 "true"s over poss  *)
      let k = A.length poss in
      let a = A.make (choose k 2 + k) [] in
      let idx = ref 0 in
      (* printf p"k = %d" k;
      printf p"poss = %{int array}" poss; *)
      for i = 0 to A.length poss - 1 do
         let c = poss.(i) in
         (* printf p"%{int list}" [c]; *)
         a.(!idx) <- [c];
         incr idx;
         for j = i + 1 to k - 1 do
            let c' = poss.(j) in
            (* printf p"%{int list}" [c;c']; *)
            a.(!idx) <- [c;c'];
            incr idx
         done
      done;
      assert (!idx = A.length a);
      a
   | 1 -> (* distribute up to 1 "true" over poss *)
      let k = A.length poss in
      let a = A.init (k+1) (fun i -> if i = 0 then [] else [poss.(i-1)]) in
      a
   | _ -> [||]
;;
let sample_gamma sub =
   let probs = A.make 2 0.0 in
   add_sub sub;
   probs.(0) <- gammas_ll () +. gammas_prior ();
   rem_sub sub;
   probs.(1) <- gammas_ll () +. gammas_prior ();
   logs_to_probs probs;
   if sample probs = 0 then
      add_sub sub
;;
let sample_gammas_ug u =
   begin try
      let poss = aol d.mapping1.(u) in
      let must = Hs.to_array d.must1.(u) in
      let remain = A.filter (fun h -> not (A.mem h must)) poss in
      if A.length remain = 0 then
         raise Done;
      if A.length remain > 12 then begin
         A.iter (fun h -> sample_gamma (Sub (u,h))) remain;
         raise Done
      end;
      A.iter (fun h -> rem_sub (Sub (u,h))) remain;
      let most = 2 - A.length must in 
      if most < 1 then
         raise Done;
      let a = gammas_possible remain most in
      let probs = A.map (fun l -> 
         L.iter (fun h -> add_sub (Sub (u,h))) l; 
         let v = gammas_ll () +. gammas_prior () in
         L.iter (fun h -> rem_sub (Sub (u,h))) l;
         v
      ) a in
      logs_to_probs probs;
      let resi = sample probs in
      L.iter (fun h -> add_sub (Sub (u,h))) a.(resi);
   with Done -> () end
;;
let sample_gammas_hb h =
   begin try
      let poss = aol d.mapping2.(h) in
      let must = Hs.to_array d.must2.(h) in
      let remain = A.filter (fun u -> not (A.mem u must)) poss in
      if A.length remain = 0 then 
         raise Done;
      if A.length remain > 12 then begin
         A.iter (fun u -> sample_gamma (Sub (u,h))) remain;
         raise Done
      end;
      A.iter (fun u -> rem_sub (Sub (u,h))) remain; 
      let most = 2 - A.length must in 
      if most < 1 then 
         raise Done;
      let a = gammas_possible remain most in
      let probs = A.map (fun l -> 
         let sub_list = L.map (fun u -> Sub (u,h)) l in
         L.iter add_sub sub_list;
         let ll = gammas_ll () in
         let lp = gammas_prior () in
         (* printf p"hb %c: %{edit list} ll = %f lp = %f" (d.decode_hb_chr h) sub_list ll lp; *)
         L.iter rem_sub sub_list;
         (ll +. lp)
      ) a in
      (* printf p"probs = %{float array}" probs; *)
      logs_to_probs probs;
      let resi = sample probs in
      L.iter (fun u -> add_sub (Sub (u,h))) a.(resi);
   with Done -> () end
;;
let sample_gammas_ug_hb u h =
   begin try
      let poss1 = aol d.mapping1.(u) in
      let poss2 = aol d.mapping2.(h) in
      let must1 = Hs.to_array d.must1.(u) in
      let must2 = Hs.to_array d.must2.(h) in
      let remain1 = A.filter (fun h -> not (A.mem h must1)) poss1 in
      let remain2 = A.filter (fun u -> not (A.mem u must2)) poss2 in
      if A.length remain1 > 12 || A.length remain2 > 12 then raise (Invalid_argument "hello");
      if A.length remain1 = 0 && A.length remain2 = 0 then
         raise Done;
      A.iter (fun h' -> rem_sub (Sub (u,h'))) remain1;
      A.iter (fun u' -> rem_sub (Sub (u',h))) remain2;
      let most1 = 2 - A.length must1 in
      let most2 = 2 - A.length must2 in 
      let a1 = gammas_possible remain1 most1 in
      let a2 = gammas_possible remain2 most2 in
      let a = [?A : (l1,l2) | l1 <- A : a1; l2 <- A : a2; (L.mem h l1 && L.mem u l2) or not (L.mem h l1 or L.mem u l2)] in
      if A.length a = 0 then 
         raise Done;
      let probs = A.map (fun (l1,l2) ->
         L.iter (fun h' -> add_sub (Sub (u,h'))) l1;
         L.iter (fun u' -> add_sub (Sub (u',h))) l2; 
         let v = gammas_ll () +. gammas_prior () in
         L.iter (fun h' -> rem_sub (Sub (u,h'))) l1;
         L.iter (fun u' -> rem_sub (Sub (u',h))) l2; 
         v
      ) a in
      (* printf p"prob-logs = %{float array}" probs; *)
      logs_to_probs probs;
      (* printf p"probs = %{float array}" probs; *)
      let resi = sample probs in
      let (l1,l2) = a.(resi) in
      L.iter (fun h' -> add_sub (Sub (u,h'))) l1;
      L.iter (fun u' -> add_sub (Sub (u',h))) l2; 
   with Done -> () end
;;

let clear_counts () = 
   Ht.clear d.edit_count;
   Ht.clear d.best_edit_count;
   Ht.clear d.true_edit_count;
   Ht.clear d.mrf_count;
   Ht.clear d.best_mrf_count;
   Ht.clear d.best_mrf_count2;
   Ht.clear d.tbls;
   Ht.clear d.pos_count;
   Ht.clear d.mrf_sum;
   Ht.clear d.pos_affix_count;
   Ht.clear d.pos_affix_sum;
   d.sub_sum <- 0.;
   d.ins_sum <- 0.;
   d.del_sum <- 0.;
   d.true_sub_sum <- 0.;
   d.true_ins_sum <- 0.;
   d.true_del_sum <- 0.;
;;
let write_gammas ~best () =
   let counts = if best then d.best_edit_count else d.edit_count in
   let out_fn = if best then "/best_edits_" else "/edits_" in
   let f u h =
      if not (L.mem h d.mapping1.(u)) then "-"
      else
         let sub = Sub (u,h) in
         let count = soi (iof (Ht.count' counts sub)) in 
         if Ht.get d.gammas sub false then "*" ^ count else count
   in
   let ugs = L.range 1 (d.num_ug_chr + 1) in
   let hbs = L.range 1 (d.num_hb_chr + 1) in
   let mtrx = L.map (fun u -> L.map (f u) hbs) ugs in
   let max_size = L.fold_left (fun mx e -> L.fold_left (fun mx' s -> max mx' (S.length s)) mx e) 0 mtrx in
   let out = open_out ~mode:[`trunc;`text;`create] (output_dir ^ out_fn ^ soi !r ^ ".counts") in
   let header = S.join "" (L.map (fun h -> S.lpad (max_size + 1) (soc (d.decode_hb_chr h))) hbs) in
   output_string out ("  " ^  header ^ "\n");
   L.iter2 (fun u_list u ->
      let u_str = S.join "" (L.map (S.lpad (max_size + 1)) u_list) in 
      output_string out (soc (d.decode_ug_chr u) ^ " " ^ u_str ^ "\n")
   ) mtrx ugs;
   
   output_string out "\n";
   foreach (L.enum d.edits) (fun ed -> match ed with 
      | Del _ | Ins _  -> 
         let n = iof (Ht.count' counts ed) in 
         if n > 0 then 
            fprintf out p"%{edit}\t%8d\n" ed n;
      | _ -> ()
   );
   
   flush out;
   close_out out;
;;

let init_gammas1_debug () =
   foreach (L.enum d.edits) (function 
      | Sub (u,h) -> Ht.replace d.gammas (Sub (u,h)) false 
      | End       -> Ht.replace d.gammas End false 
      | Del u     -> Ht.replace d.gammas (Del u) false
      | Ins h     -> Ht.replace d.gammas (Ins h) (h = L.nth d.mapping3 0 || h = L.nth d.mapping3 1));

   d.mapping1.(d.encode_ug_chr '<') <- [d.encode_hb_chr '<'];
   d.mapping1.(d.encode_ug_chr 'H') <- [d.encode_hb_chr 'H'];
   d.mapping1.(d.encode_ug_chr 'i') <- [d.encode_hb_chr 'a'];
   d.mapping1.(d.encode_ug_chr 'k') <- [d.encode_hb_chr 'k'];
   d.mapping1.(d.encode_ug_chr 'm') <- [d.encode_hb_chr 'm'];
   d.mapping1.(d.encode_ug_chr 'n') <- [d.encode_hb_chr 'n'];
   d.mapping1.(d.encode_ug_chr 'q') <- [d.encode_hb_chr 'q'];
   d.mapping1.(d.encode_ug_chr 'r') <- [d.encode_hb_chr 'r'];

   Ht.clear fsa_cache;
   (* rebuild mapping2 *)
   foreach (1 -- d.num_hb_chr) (fun h -> d.mapping2.(h) <- []);
   foreach (1 -- d.num_ug_chr) (fun u -> L.iter (fun h -> d.mapping2.(h) <- u :: d.mapping2.(h)) d.mapping1.(u));
   
   foreach (1 -- d.num_ug_chr) (fun u -> 
      match d.mapping1.(u) with [h] -> 
         begin
            Hs.add d.must1.(u) h;
            Hs.add d.must2.(h) u;
         end
      | _ -> ());
      
   foreach (1 -- d.num_hb_chr) (fun h -> 
      match d.mapping2.(h) with [u] -> 
         begin
            Hs.add d.must1.(u) h;
            Hs.add d.must2.(h) u;
         end
      | _ -> ());

   clear_counts ();
;;
let init_gammas1 () =
   foreach (L.enum d.edits) (function 
      | Sub (u,h) -> Ht.replace d.gammas (Sub (u,h)) false 
      | End       -> Ht.replace d.gammas End false 
      | Del u     -> Ht.replace d.gammas (Del u) false
      | Ins h     -> Ht.replace d.gammas (Ins h) (h = L.nth d.mapping3 0 || h = L.nth d.mapping3 1));
   

   for u = 1 to d.num_ug_chr do
      let hbs = aol d.mapping1.(u) in
      if A.length hbs = 1 then
         ()
         (* Ht.replace d.gammas (Sub (u,hbs.(0))) true *)
      else begin
         let counts = A.map (fun h -> Ht.count' d.edit_count (Sub (u,h))) hbs in
         let i,mx = max_itemi counts in
         let mx2 = max_item (A.filteri (fun j _ -> j <> i) counts) in
         let ratio = mx /. mx2 in
         if ratio > 5.0 && not use_english then begin
            let super_h = hbs.(i) in
            d.mapping1.(u) <- [super_h];
            (* foreach (A.enum hbs) (fun h -> Ht.replace d.gammas (Sub (u,h)) false);
            Ht.replace d.gammas (Sub (u,super_h)) true; *)
         end
      end
   done;
   Ht.clear fsa_cache;
   (* rebuild mapping2 *)
   foreach (1 -- d.num_hb_chr) (fun h -> d.mapping2.(h) <- []);
   foreach (1 -- d.num_ug_chr) (fun u -> L.iter (fun h -> d.mapping2.(h) <- u :: d.mapping2.(h)) d.mapping1.(u));
   
   foreach (1 -- d.num_ug_chr) (fun u -> 
      match d.mapping1.(u) with [h] -> 
         begin
            Hs.add d.must1.(u) h;
            Hs.add d.must2.(h) u;
         end
      | _ -> ());
      
   foreach (1 -- d.num_hb_chr) (fun h -> 
      match d.mapping2.(h) with [u] -> 
         begin
            Hs.add d.must1.(u) h;
            Hs.add d.must2.(h) u;
         end
      | _ -> ());

   clear_counts ();
;;

let init_gammas2 () =
   let counts_u = Ht.create 50 in
   let counts_h = Ht.create 50 in
   foreach (1 -- d.num_ug_chr) (fun u -> 
      let (counts : float array) = A.init (d.num_hb_chr + 1) (fun h -> if h = 0 then 0. else Ht.count' d.edit_count (Sub (u,h))) in
      Ht.add counts_u u counts);
   foreach (1 -- d.num_hb_chr) (fun h ->
      let (counts : float array) = A.init (d.num_ug_chr + 1) (fun u -> if u = 0 then 0. else Ht.count' d.edit_count (Sub (u,h))) in
      Ht.add counts_h h counts);
   
   let three_allowed = ref 0 in
   foreach (1 -- d.num_hb_chr) (fun h ->
      let rest = L.filter (fun u -> not (Hs.mem d.must2.(h) u)) d.mapping2.(h) in
      let percents = L.map (fun u -> let counts = Ht.find counts_u u in counts.(h) /. (A.sum counts)) rest in
      let combined = aol (L.combine percents rest) in
      A.sort (fun x y -> - compare (fst x) (fst y)) combined; 
      (* printf p"%d counts = %{(float * int) array}" h combined; *)
      let allowed = if !three_allowed > 0 then 3 else 2 in
      let sofar = ref 0 in
      let must = Hs.to_list d.must2.(h) in
      foreach (L.enum must) (fun u ->
         let edit = Sub (u,h) in
         (* printf p"must: %{edit}" edit; *)
         Ht.replace d.gammas edit true;
         incr sofar);
      let index = ref 0 in
      while (!sofar < allowed && !index < A.length combined) do
         let (pct,u) = combined.(!index) in
         incr index;
         (* if pct > 0.09 then begin *)
         if true then begin
            let edit = Sub (u,h) in
            (* printf p"> 0.09: %{edit}" edit; *)
            Ht.replace d.gammas edit true;
            incr sofar;
         end
      done;
      assert (if !three_allowed > 0 then !sofar <= 3 else (!sofar <= 2 || (!sofar = 3 && h = 7))); 
      if !sofar = 2 then decr three_allowed);

   foreach (1 -- d.num_ug_chr) (fun u ->
      let hbs = L.filter (fun h -> Ht.find d.gammas (Sub (u,h))) d.mapping1.(u) in
      let counts = L.map (fun h -> Ht.count' d.edit_count (Sub (u,h))) hbs in
      let combined = aol (L.combine counts hbs) in
      A.sort (fun x y -> - compare (fst x) (fst y)) combined;
      assert (!_DEBUG || A.length combined > 0 || use_english || not use_morph);
      if A.length combined = 0 then
         Ht.replace d.gammas (Sub (u, L.hd d.mapping1.(u))) true;
      A.iter (fun (_,h) -> Ht.replace d.gammas (Sub (u,h)) false) (A.slice ~first:2 combined));
   
   let alpha_sum = ref gamma_sub_false in (* for End *)
   for u = 1 to d.num_ug_chr do
      L.iter (fun h -> 
         alpha_sum := !alpha_sum +. (if Ht.find d.gammas (Sub (u,h)) then gamma_sub_true else gamma_sub_false);
         ) 
      d.mapping1.(u)
   done;
   d.alphas_sub_sum <- !alpha_sum;
   d.alphas_ins_sum <- L.fold_left 
      (fun acc h -> acc +. (if Ht.get d.gammas (Ins h) false then gamma_ins_true else gamma_ins_false)) 0. d.mapping3;
   printf p"gamma init log-prior:  %f" (gammas_prior ());
;;


let decrement_entry_counts ?(best=true) entry i =
   d.n <- d.n -. 1.;
   decr cogs;
   let opos = entry.pos in
   let opre = entry.word_ug.pre in
   let ostm = entry.word_ug.stm in
   let osuf = entry.word_ug.suf in
   
   let hb_pre = entry.word_hb.pre in
   let hb_suf = entry.word_hb.suf in
   
   if A.length hb_pre > 0 then begin
      assert (Ht.find d.pos_affix_sum (opos, Pre) > 0.);
      assert (Ht.find d.pos_affix_count (hb_pre, opos, Pre) > 0.);      
      Ht.decr' d.pos_affix_sum (opos, Pre);
      Ht.decr' d.pos_affix_count (hb_pre, opos, Pre)
   end;
   if A.length hb_suf > 0 then begin
      assert (Ht.find d.pos_affix_sum (opos, Suf) > 0.);
      (* printf p"hebrew suffix: %s (%s)" (d.decode_hb hb_suf) opos; *)
      assert (Ht.find d.pos_affix_count (hb_suf, opos, Suf) > 0.);      
      Ht.decr' d.pos_affix_sum (opos, Suf);
      Ht.decr' d.pos_affix_count (hb_suf, opos, Suf)
   end;
   
   let pre_tbls = get_morph_tables opre Pre opos in
   let stm_tbls = get_morph_tables ostm Stm opos in
   let suf_tbls = get_morph_tables osuf Suf opos in
   
   (* if i = 136 then begin
      let a = Da.to_array (Ht.find stm_tbls entry.stm_ed) in      
      printf p"(before decr) tbls = %{int array array}" (A.map Hs.to_array a);
   end; *)
   let pre_count = get_morph_counts opre Pre opos in
   let stm_count = get_morph_counts ostm Stm opos in
   let suf_count = get_morph_counts osuf Suf opos in
   let best_pre_count = get_best_morph_counts opre Pre opos in
   let best_stm_count = get_best_morph_counts ostm Stm opos in
   let best_suf_count = get_best_morph_counts osuf Suf opos in
   
   let mrf_sum = d.mrf_sum in
   let pre_key = (opre,opos,Pre) in
   let stm_key = (ostm,opos,Stm) in
   let suf_key = (osuf,opos,Suf) in
   
   let pre_tbl = entry.pre_tbl in
   let stm_tbl = entry.stm_tbl in
   let suf_tbl = entry.suf_tbl in

   let pre_ed  = entry.pre_ed in
   let stm_ed  = entry.stm_ed in
   let suf_ed  = entry.suf_ed in
   let best_pre_ed  = entry.best_pre_ed in
   let best_stm_ed  = entry.best_stm_ed in
   let best_suf_ed  = entry.best_suf_ed in
   
   let decr_ed ed =
      Ht.decr' d.edit_count ed;
      match ed with
         | Sub _ | End -> d.sub_sum <- d.sub_sum -. 1.
         | Ins _ -> d.ins_sum <- d.ins_sum -. 1.
         | Del _ -> d.del_sum <- d.del_sum -. 1.
   in
   let decr_true_ed ed =
      Ht.decr' d.true_edit_count ed;
      match ed with
         | Sub _ | End -> d.true_sub_sum <- d.true_sub_sum -. 1.
         | Ins _ -> d.true_ins_sum <- d.true_ins_sum -. 1.
         | Del _ -> d.true_del_sum <- d.true_del_sum -. 1.
   in
   
   Hs.remove pre_tbl i;
   Hs.remove stm_tbl i;
   Hs.remove suf_tbl i;

   if Hs.length pre_tbl = 0 then (
      L.iter decr_ed pre_ed;
      Da.remove (Ht.find pre_tbls pre_ed) pre_tbl);
   
   if Hs.length stm_tbl = 0 then (
      L.iter decr_ed stm_ed;
      Da.remove (Ht.find stm_tbls stm_ed) stm_tbl);
      
   if Hs.length suf_tbl = 0 then (
      L.iter decr_ed suf_ed;
      Da.remove (Ht.find suf_tbls suf_ed) suf_tbl);
   
   L.iter decr_true_ed pre_ed;
   L.iter decr_true_ed stm_ed;
   L.iter decr_true_ed suf_ed;

   Ht.decr' pre_count pre_ed;
   Ht.decr' stm_count stm_ed;
   Ht.decr' suf_count suf_ed;
      
   if best then begin
      Ht.decr' best_pre_count best_pre_ed;
      Ht.decr' best_stm_count best_stm_ed;
      Ht.decr' best_suf_count best_suf_ed
   end;   
   
   Ht.decr' mrf_sum pre_key;
   Ht.decr' mrf_sum stm_key;
   Ht.decr' mrf_sum suf_key;
   Ht.decr' d.pos_count opos;

   (* if i = 136 then begin
      let a = Da.to_array (Ht.find stm_tbls entry.stm_ed) in      
      printf p"(after decr) tbls = %{int array array}" (A.map Hs.to_array a);
   end; *)
      
   assert (Ht.find pre_count pre_ed >= 0.);
   assert (Ht.find stm_count stm_ed >= 0.);
   assert (Ht.find suf_count suf_ed >= 0.);
   assert (Ht.find mrf_sum pre_key >= 0.);
   assert (Ht.find mrf_sum stm_key >= 0.);
   assert (Ht.find mrf_sum suf_key >= 0.);
   assert (Ht.find d.pos_count opos >= 0.);
   
   let pre_mrf_c = Ht.find pre_count pre_ed in
   let stm_mrf_c = Ht.find stm_count stm_ed in
   let suf_mrf_c = Ht.find suf_count suf_ed in

   let pre_tbl_c = Da.fold_left (fun acc hs -> acc +. foi (Hs.length hs)) 0. (Ht.find pre_tbls pre_ed) in
   let stm_tbl_c = Da.fold_left (fun acc hs -> acc +. foi (Hs.length hs)) 0. (Ht.find stm_tbls stm_ed) in
   let suf_tbl_c = Da.fold_left (fun acc hs -> acc +. foi (Hs.length hs)) 0. (Ht.find suf_tbls suf_ed) in

   (* printf p"(%d) PRE after decr: mrf_c = %d     tbl_c = %d" i (iof pre_mrf_c) (iof pre_tbl_c);
   printf p"(%d) STM after decr: mrf_c = %d     tbl_c = %d" i (iof stm_mrf_c) (iof stm_tbl_c);
   printf p"(%d) SUF after decr: mrf_c = %d     tbl_c = %d" i (iof suf_mrf_c) (iof suf_tbl_c); *)

   assert (pre_mrf_c = pre_tbl_c);
   assert (stm_mrf_c = stm_tbl_c);
   assert (suf_mrf_c = suf_tbl_c);
;;
let increment_pos_counts entry =
   (* increment counts *)
   d.n <- d.n +. 1.;
   incr cogs;
   let npos = entry.pos in
   let nseg = entry.word_ug in
   let tbls = d.tbls in
   let mrf_count  = d.mrf_count in
   let mrf_sum    = d.mrf_sum in
   let pre_key = (nseg.pre,npos,Pre) in
   let stm_key = (nseg.stm,npos,Stm) in
   let suf_key = (nseg.suf,npos,Suf) in
   Ht.incr' d.pos_count npos;
   Ht.incr' mrf_sum pre_key;
   Ht.incr' mrf_sum stm_key;
   Ht.incr' mrf_sum suf_key;
   if not (Ht.mem tbls pre_key) then Ht.add tbls pre_key (Ht.create 200);
   if not (Ht.mem tbls stm_key) then Ht.add tbls stm_key (Ht.create 200);
   if not (Ht.mem tbls suf_key) then Ht.add tbls suf_key (Ht.create 200);
      
   if not (Ht.mem mrf_count pre_key) then Ht.add mrf_count pre_key (Ht.create 200);
   if not (Ht.mem mrf_count stm_key) then Ht.add mrf_count stm_key (Ht.create 200);
   if not (Ht.mem mrf_count suf_key) then Ht.add mrf_count suf_key (Ht.create 200);

   let hb_pre = entry.word_hb.pre in
   let hb_suf = entry.word_hb.suf in
   
   if A.length hb_pre >= 0 then begin
      Ht.incr' d.pos_affix_sum (npos, Pre);
      Ht.incr' d.pos_affix_count (hb_pre, npos, Pre)
   end;
   if A.length hb_suf >= 0 then begin
      Ht.incr' d.pos_affix_sum (npos, Suf);
      Ht.incr' d.pos_affix_count (hb_suf, npos, Suf)
   end;
;;
let increment_mrf_counts ?(best=false) entry mtype i =
   let pos     = entry.pos in
   let nmorf   = match mtype with Pre -> entry.word_ug.pre  | Stm -> entry.word_ug.stm    | Suf -> entry.word_ug.suf in
   let nedits  = match mtype with Pre -> entry.pre_ed       | Stm -> entry.stm_ed         | Suf -> entry.suf_ed in
   let bedits  = match mtype with Pre -> entry.best_pre_ed  | Stm -> entry.best_stm_ed    | Suf -> entry.best_suf_ed in
   let tbl     = match mtype with Pre -> entry.pre_tbl      | Stm -> entry.stm_tbl        | Suf -> entry.suf_tbl in
   let tbls       = Ht.find d.tbls (nmorf,pos,mtype) in 
   let mrf_count  = Ht.find d.mrf_count (nmorf,pos,mtype) in

   (* if i = 136 && mtype = Stm && Ht.mem tbls nedits then begin
      let a = Da.to_array (Ht.find tbls nedits) in
      printf p" (before incr) tbls = %{int array array}" (A.map Hs.to_array a);
   end; *)
   (* if (Hs.length tbl != 0) then assert (Hs.exists (fun hs -> hs == tbl) (Ht.find tbls nedits)); *)
      
   let incr_ed ed =
      Ht.incr' d.edit_count ed;
      match ed with
         | Sub _ | End -> d.sub_sum <- d.sub_sum +. 1.
         | Ins _ -> d.ins_sum <- d.ins_sum +. 1.
         | Del _ -> d.del_sum <- d.del_sum +. 1.
   in
   let incr_true_ed ed =
      Ht.incr' d.true_edit_count ed;
      match ed with
         | Sub _ | End -> d.true_sub_sum <- d.true_sub_sum +. 1.
         | Ins _ -> d.true_ins_sum <- d.true_ins_sum +. 1.
         | Del _ -> d.true_del_sum <- d.true_del_sum +. 1.
   in
   
   Ht.incr' mrf_count nedits;
   if best then 
      (let best_mrf_count = get_best_morph_counts nmorf mtype pos in Ht.incr' best_mrf_count bedits);
   (* if i = 136 && mtype = Stm then printf p"before = %d" (Hs.length tbl); *)
   Hs.add tbl i;
   if Hs.length tbl = 1 then (
      L.iter incr_ed nedits;
      if not (Ht.mem tbls nedits) then
         Ht.add tbls nedits (Da.make 50);
      let da = Ht.find tbls nedits in
      assert (not (Da.memq da tbl));
      Da.add da tbl);
      
   (* if i = 136 then printf p"after = %d (%d)" (Hs.length tbl) (Hs.length (Da.get (Ht.find tbls nedits) 0)); *)
   let mrf_c = Ht.count' mrf_count nedits in
   let tbl_c = Da.fold_left (fun acc hs -> acc +. foi (Hs.length hs)) 0. (Ht.find tbls nedits) in
   (* if i = 136 && mtype = Stm then printf p"(%d) after incr: mrf_c = %d     tbl_c = %d" i (iof mrf_c) (iof tbl_c);
   if i = 136 && mtype = Stm then printf p"tbl = %{int array}" (Hs.to_array tbl);
   let a = Da.to_array (Ht.find tbls nedits) in
   if i = 136 && mtype = Stm then printf p"(after incr) tbls = %{int array array}" (A.map Hs.to_array a); *)
   assert (mrf_c = tbl_c);
   L.iter incr_true_ed nedits;

   (* let n = foi (Hs.length indices1) in
   printf p"(%d) n = %d,  count = %d" i (iof n) (iof $ Ht.count' mrf_counts nedits);
   assert (n = Ht.count' mrf_counts nedits); *)
;;
let actual_hb_freq mrf pos mtype =
   Ht.find d.hb_freq (mrf, pos, mtype)
;;
let implied_hb_freq mrf pos mtype num = 
   (* always assumes that 'num' added (for the instance(s) being sampled *)
   let numer = Ht.get d.pos_affix_count (mrf, pos, mtype) 0. +. num in
   let denom = Ht.get d.pos_affix_sum (pos, mtype) 0. +. num in
   assert (num >= 1.);
   assert (numer >= 1.);
   assert (denom >= 1.);
   numer /. denom
;;

let morph_prob ?(num=1.) ?(base=false) ?(true_count=false) ug_mrf hb_mrf edits pos mtype = 
   let counts = get_morph_counts ug_mrf mtype pos in
   let pos_n = Ht.count' d.pos_count pos in
   let alpha = match mtype with Pre -> alpha_pre | Stm -> alpha_stm | Suf -> alpha_suf in
   let n = if mtype = Stm then d.n else pos_n in
   let num_ins = ref 0 in
   let num_del = ref 0 in
   let edits_prob = L.fold_left (fun accum edit -> 
      begin match edit with Ins _ -> incr num_ins | Del _ -> incr num_del | _ -> () end;
      edit_prob ~true_count edit *. accum) 1. edits 
   in
   let ins_pen = match !num_ins with 0 -> (1. -. ins_prior) | 1 -> ins_prior /. foi (A.length ug_mrf + 1) | _ -> 0. in
   let del_pen = match !num_del with 0 -> (1. -. del_prior) | 1 -> del_prior /. foi (A.length ug_mrf) | _ -> 0. in
   let pen = ins_pen *. del_pen in

   let freq_match = 
      if A.length hb_mrf < 0 or !init or not (use_freq) or mtype = Stm then 1.0 
      else begin
         printf p"frequency of '%s' (%s)" (d.decode_hb hb_mrf) pos;
         let hb_freq1 = actual_hb_freq hb_mrf pos mtype in
         let hb_freq2 = implied_hb_freq hb_mrf pos mtype num in
         Gsl_randist.gaussian_pdf (hb_freq1 -. hb_freq2) 0.06
      end 
   in
   let base_prob = edits_prob *. pen in

   (* if mtype = Suf then begin
      if d.encode_ug "m" = ug_mrf && d.encode_hb "ym" = hb_mrf && num > 1. then begin
         let hb_freq1 = actual_hb_freq hb_mrf pos mtype in
         let hb_freq2 = implied_hb_freq hb_mrf pos mtype num in
         printf p"base prob : num = %d,  (m,ym) = %f   =  %f * %{float list}, freq =  %f, %f ... %f" (iof num) base_prob pen (L.map edit_prob edits) hb_freq2 hb_freq1 (base_prob *. freq_match);
      end; 
      if d.encode_ug "m" = ug_mrf && d.encode_hb "m" = hb_mrf && num > 1. then begin
         let hb_freq1 = actual_hb_freq hb_mrf pos mtype in
         let hb_freq2 = implied_hb_freq hb_mrf pos mtype num in
         printf p"base prob : num = %d,  (m,m) = %f    =  %f * %{float list}, freq =  %f, %f ... %f" (iof num) base_prob pen (L.map edit_prob edits) hb_freq2 hb_freq1 (base_prob *. freq_match);
      end
   end; *)
   if base then 
      base_prob *.freq_match
   else 
      ((alpha *. base_prob /. (n +. alpha))  +.  (Ht.count' counts edits /. (n +. alpha))) *. freq_match
;;
let sample_source ug_mrf edits pos mtype =
   let tbls_ = get_morph_tables ug_mrf mtype pos in
   if not (Ht.mem tbls_ edits) then (Hs.create 50)
   else begin
      let tbls = Ht.find tbls_ edits in
      let pos_n = Ht.count' d.pos_count pos in
      let alpha = match mtype with Pre -> alpha_pre | Stm -> alpha_stm | Suf -> alpha_suf in
      let n = if mtype = Stm then d.n else pos_n in
      let edits_prob = L.fold_left (fun accum edit -> edit_prob edit *. accum) 1. edits in
      let res = A.make (Da.length tbls + 1) (Hs.create 50) in
      let i = ref 1 in
      Da.iter (fun hs -> res.(!i) <- hs; incr i) tbls;
      assert (!i = Da.length tbls + 1);
      let probs = A.init 
         (A.length res) 
         (fun i -> if i = 0 then alpha *. edits_prob /. (n +. alpha) else foi (Hs.length res.(i)) /. (n +. alpha)) in
      let resi = sample probs in
      res.(resi)
   end
;;

let get_possible_edits ug_mrf pos mtype =
   try 
      Ht.find possible_edits_cache (ug_mrf,pos,mtype)
   with Not_found ->
      let fsa = get_fsa ug_mrf pos mtype in
      let edit_lists = 
         if Obj.magic (Fst.get_start fsa) = -1 then [||]
      else
         A.map (fun edits -> L.map int_to_edit (loa edits)) (Fst.get_all_edits fsa) in
      Ht.add possible_edits_cache (ug_mrf,pos,mtype) edit_lists;
      edit_lists
;;
let all_possible_predictions i entry =
   if i mod 100 = 0 then
      printf p"getting all possible:  %d" i;

   let res = ref [] in
   let num_segs = A.length entry.segments_ug in
   let num_pos = A.length d.poses in
   for j = 0 to num_segs * num_pos - 1 do
      let pos = d.poses.(j mod num_pos) in
      let seg = entry.segments_ug.(j / num_pos) in
      let pre_fsa = get_fsa seg.pre pos Pre in
      let stm_fsa = get_fsa seg.stm pos Stm in
      let suf_fsa = get_fsa seg.suf pos Suf in
      if L.for_all (fun fsa -> Obj.magic (Fst.get_start fsa) <> -1) [pre_fsa; stm_fsa; suf_fsa] then
         let pres = Fst.get_all_edits pre_fsa in
         let stms = Fst.get_all_edits stm_fsa in
         let sufs = Fst.get_all_edits suf_fsa in
         assert (A.length pres > 0);
         assert (A.length stms > 0);
         assert (A.length sufs > 0);
         for i = 0 to A.length pres - 1 do
            for j = 0 to A.length stms - 1 do
               for k = 0 to A.length sufs - 1 do
                  let pre = L.map int_to_edit (loa pres.(i)) in
                  let stm = L.map int_to_edit (loa stms.(j)) in
                  let suf = L.map int_to_edit (loa sufs.(k)) in
                  res := { p_ug = seg; 
                           p_hb = {pre = hb_from_edits pre; stm = hb_from_edits stm; suf = hb_from_edits suf};
                           p_pre_ed = pre;
                           p_stm_ed = stm;
                           p_suf_ed = suf;
                           p_pos = pos } :: !res
               done
            done
         done
   done;
   entry.all_possible <- aol !res;
   entry.all_probs <- A.make (A.length entry.all_possible) 0.
;;

let get_best_with_uh u h e =
   let all_possible = e.all_possible in
   let noncog_prob = exp (get_noncog_logprob e.word_ug) in
   if A.length all_possible = 0 then
      (noncog_prob, None)
   else
      let forbidden = aol $ L.map_filter (fun h' -> if h' <> h then Some (Sub (u,h')) else None) d.mapping1.(u) in
      let probs = A.map (begin fun p ->
            let ug_pre = p.p_ug.pre in
            let ug_stm = p.p_ug.stm in
            let ug_suf = p.p_ug.suf in
            let hb_pre = p.p_hb.pre in
            let hb_stm = p.p_hb.stm in
            let hb_suf = p.p_hb.suf in            
            let pre_ed = p.p_pre_ed in
            let stm_ed = p.p_stm_ed in
            let suf_ed = p.p_suf_ed in
            let pos = p.p_pos in
            let has_forbidden el = L.exists (fun ed -> A.mem ed forbidden) el in
            if has_forbidden pre_ed || has_forbidden stm_ed || has_forbidden suf_ed then neg_infinity
            else 
               morph_prob ~base:false ug_pre hb_pre pre_ed pos Pre +.
               morph_prob ~base:false ug_stm hb_stm stm_ed pos Stm +.
               morph_prob ~base:false ug_suf hb_suf suf_ed pos Suf +. log cog_prior
         end) all_possible in
      
      let maxi, max_prob = max_itemi probs in
      if max_prob < noncog_prob then
         (noncog_prob, None)
      else
         (max_prob, Some maxi)
;;
let propose u =
   let poss_hb = aol d.mapping1.(u) in
   let trues = A.fold_left (fun acc h -> if Ht.find d.gammas (Sub (u,h)) then acc + 1 else acc) 0 poss_hb in
   if trues = 1 then begin
      let entries = char_index.(u) in

      (* A.iter (fun i -> let e = d.entries.(i) in if e.cog then decrement_entry_counts e i) entries;
      let current_logprob = ref (gammas_prior ()) in
      A.iter (fun i -> 
         let e = d.entries.(i) in 
         if e.cog then begin
            current_logprob := !current_logprob +. log cog_prior
               +. log (morph_prob ~base:false e.ug_word.pre e.hb_word.pre e.pos Pre)
               +. log (morph_prob ~base:false e.ug_word.stm e.hb_word.stm e.pos Stm)
               +. log (morph_prob ~base:false e.ug_word.suf e.hb_word.suf e.pos Suf);
            increment_pos_counts entry;
            increment_mrf_counts entry Pre i;
            increment_mrf_counts entry Stm i;
            increment_mrf_counts entry Suf i
         end else begin
            let len = A.length e.word_ug.pre + A.length e.word_ug.stm + A.length e.word_ug.suf + 1 in
            current_logprob := !current_logprob +. log (1. -. cog_prior) +. foi len *. (log 1. -. log 31.)
         end
      ) entries; *)
   
      A.iter (fun h -> Ht.replace d.gammas (Sub (u,h)) false) poss_hb;
      A.iter (fun i -> let e = d.entries.(i) in if e.cog then decrement_entry_counts e i) entries;

      let probs = A.map (fun h -> 
         Ht.replace d.gammas (Sub (u,h)) true;
         let res = ref (gammas_prior ()) in
         A.iter (fun i ->
            let e = d.entries.(i) in
            (match get_best_with_uh u h e with
               | (pr, None) -> 
                  res := !res +. pr;
                  e.cog <- false;
               | (pr, Some idx) -> 
                  res := !res +. pr;
                  let predict = e.all_possible.(idx) in
                  e.cog         <- true;
                  e.word_ug     <- predict.p_ug;
                  e.word_hb.pre <- predict.p_hb.pre;
                  e.word_hb.stm <- predict.p_hb.stm;
                  e.word_hb.suf <- predict.p_hb.suf;
                  e.pos         <- predict.p_pos;
                  e.pre_ed      <- predict.p_pre_ed;
                  e.stm_ed      <- predict.p_stm_ed;
                  e.suf_ed      <- predict.p_suf_ed;

                  e.pre_tbl     <- sample_source e.word_ug.pre e.pre_ed e.pos Pre;
                  e.stm_tbl     <- sample_source e.word_ug.stm e.stm_ed e.pos Stm;
                  e.suf_tbl     <- sample_source e.word_ug.suf e.suf_ed e.pos Suf;
                  
                  increment_pos_counts e;
                  increment_mrf_counts e Pre i;
                  increment_mrf_counts e Stm i;
                  increment_mrf_counts e Suf i;
            )
         ) entries; 
         A.iter (fun i -> let e = d.entries.(i) in if e.cog then decrement_entry_counts e i) entries;
         Ht.replace d.gammas (Sub (u,h)) false;
         !res
      ) poss_hb in
      
      logs_to_probs probs;
      let resi = sample probs in
      let h = poss_hb.(resi) in
      printf p"proposed: %c -> %c" (d.decode_ug_chr u) (d.decode_hb_chr h);
      
      Ht.replace d.gammas (Sub (u,h)) true;
      A.iter (fun i ->
         let e = d.entries.(i) in
         (match get_best_with_uh u h e with
            | (pr, None) -> 
               e.cog <- false;
            | (pr, Some idx) -> 
               let predict = e.all_possible.(idx) in
               e.cog         <- true;
               e.word_ug     <- predict.p_ug;
               e.word_hb.pre <- predict.p_hb.pre;
               e.word_hb.stm <- predict.p_hb.stm;
               e.word_hb.suf <- predict.p_hb.suf;
               e.pos         <- predict.p_pos;
               e.pre_ed      <- predict.p_pre_ed;
               e.stm_ed      <- predict.p_stm_ed;
               e.suf_ed      <- predict.p_suf_ed;

               e.pre_tbl     <- sample_source e.word_ug.pre e.pre_ed e.pos Pre;
               e.stm_tbl     <- sample_source e.word_ug.stm e.stm_ed e.pos Stm;
               e.suf_tbl     <- sample_source e.word_ug.suf e.suf_ed e.pos Suf;
               
               increment_pos_counts e;
               increment_mrf_counts e Pre i;
               increment_mrf_counts e Stm i;
               increment_mrf_counts e Suf i;
         )
      ) entries; 
   end
;;

let sample_gammas () = 
   let hs = A.init d.num_hb_chr (fun i -> ("hb", i+1)) in
   let us = A.init d.num_ug_chr (fun i -> ("ug", i+1)) in
   let both = A.append hs us in
   (* A.shuffle both; *)
   A.iter (fun (s,c) -> match s with 
      | "ug" ->         
         let u = c in
         let poss = [?A : h | h <- L : d.mapping1.(u); not (Hs.mem d.must1.(u) h) && Ht.find d.gammas (Sub (u,h))] in
         begin try 
            let h = poss.(Random.int (A.length poss)) in
            sample_gammas_ug_hb u h;
            (* printf p"sampling gammas: u = %c,  h = %c" (d.decode_ug_chr u) (d.decode_hb_chr h); *)
         with Invalid_argument _ -> (
            sample_gammas_ug u;             
            (* printf p"sampling gamma: u = %c" (d.decode_ug_chr u) *)
         ) end
      
      | "hb" ->
         let h = c in
         let poss = [?A : u | u <- L : d.mapping2.(h); not (Hs.mem d.must2.(h) u) && Ht.find d.gammas (Sub (u,h))] in
         begin try 
            let u = poss.(Random.int (A.length poss)) in
            sample_gammas_ug_hb u h;
            (* printf p"sampling gammas: u = %c,  h = %c" (d.decode_ug_chr u) (d.decode_hb_chr h); *)
         with Invalid_argument _ -> (
            sample_gammas_hb h;
            (* printf p"sampling gamma: h = %c" (d.decode_hb_chr h) *)
         ) end
      | _ -> assert false
   ) both;
   (* A.iter (fun u -> if A.length char_index.(u) < 170 && not !_DEBUG then propose u) (A.range 1 (d.num_ug_chr + 1)); *)
   let subs = Ht.enum d.gammas |> E.filter_map (fun (edit,b) -> if b then (match edit with Sub _ -> Some edit | _ -> None) else None) |> L.of_enum |> L.sort in
   Ht.incr gammas_record subs
;;

let sample_cog entry i =
   if not !init then begin
      entry.cog <- false;
      let cog_prob = entry.cog_prob in
      let noncog_prob = exp (get_noncog_logprob entry.word_ug) in
      if !r > burnin then begin
         entry.all_cog_prob.(0) <- entry.all_cog_prob.(0) +. cog_prob;
         entry.all_cog_prob.(1) <- entry.all_cog_prob.(1) +. noncog_prob;
         let best_cog_prob = entry.all_cog_prob.(0) *. cog_prior in
         let best_noncog_prob = entry.all_cog_prob.(1) *. (1. -. cog_prior) in
         entry.best_cog <- (best_cog_prob > best_noncog_prob);

      end;
      let a = [|cog_prob *. cog_prior; noncog_prob *. (1. -. cog_prior)|] in
      if anneal then begin
         adjust a; 
         A.update (fun v -> v ** schedule.(!r)) a;
      end;
      (* print_int i; print_string " ";
      A.print sof a; *)
      let rn = Fst.random mtrng (a.(0) +. a.(1)) in
      if rn < a.(0) then begin
         entry.cog <- true
      end
   end
;;
let sample_prediction entry i =
   if entry.cog then decrement_entry_counts entry i;

   assert (A.length entry.all_possible > 0);
   (* if A.length entry.all_possible = 0 then begin
      printf p"no possible: %d" i;
      entry.xxx <- true;
      entry.cog <- false;
      entry.best_cog <- false;
      decr cogs
   end
   else begin *)

   let probs = A.map (begin fun p -> 
         let ug_pre = p.p_ug.pre in
         let ug_stm = p.p_ug.stm in
         let ug_suf = p.p_ug.suf in
         let hb_pre = p.p_hb.pre in
         let hb_stm = p.p_hb.stm in
         let hb_suf = p.p_hb.suf in
         let pre_ed = p.p_pre_ed in
         let stm_ed = p.p_stm_ed in
         let suf_ed = p.p_suf_ed in
         let pos = p.p_pos in
         morph_prob ~base:false ug_pre hb_pre pre_ed pos Pre *.
         morph_prob ~base:false ug_stm hb_stm stm_ed pos Stm *.
         morph_prob ~base:false ug_suf hb_suf suf_ed pos Suf 
      end) entry.all_possible in
   
   (* if !r > burnin then A.iteri (fun i p -> entry.all_probs.(i) <- entry.all_probs.(i) +. p) probs; *)

   let prob_sum = A.sum probs in
   let maxi, max_prob = max_itemi probs in
   entry.cog_prob    <- max_prob;          (* may want to switch this to the max prob (viterbi) DONE *)
   sample_cog entry i;
   
   (* A.updatei (fun i p -> p +. (probs.(i) /. prob_sum)) entry.all_probs; *)
   (* entry.all_probs.(maxi) <- entry.all_probs.(maxi) +. 1.; *)
   
   if entry.cog then begin
      if anneal then (adjust probs; A.update (fun v -> v ** schedule.(!r)) probs);
      let resi = sample ~sum:(if anneal then None else Some prob_sum) probs in
      let predict = entry.all_possible.(resi) in
      entry.word_ug     <- predict.p_ug;
      entry.word_hb.pre <- predict.p_hb.pre;
      entry.word_hb.stm <- predict.p_hb.stm;
      entry.word_hb.suf <- predict.p_hb.suf;
      entry.pos         <- predict.p_pos;
      entry.pre_ed      <- predict.p_pre_ed;
      entry.stm_ed      <- predict.p_stm_ed;
      entry.suf_ed      <- predict.p_suf_ed;
      (* printf p"adding to segments_record: %d" i;  *)
      Ht.incr entry.segments_record predict.p_ug; 
      
      assert (predict.p_hb.pre = hb_from_edits predict.p_pre_ed);
      assert (predict.p_hb.stm = hb_from_edits predict.p_stm_ed);
      assert (predict.p_hb.suf = hb_from_edits predict.p_suf_ed);
      
      let pre = hb_from_edits entry.pre_ed in
      let stm = hb_from_edits entry.stm_ed in
      let suf = hb_from_edits entry.suf_ed in
      assert (pre = entry.word_hb.pre);
      assert (stm = entry.word_hb.stm);
      (* printf p"edit list = %{edit list}   hb_mrf1 = %s   hb_mrf2 = %s" entry.suf_ed (d.decode_hb suf) (d.decode_hb predict.p_hb.suf);  *)
      assert (suf = entry.word_hb.suf);
      
      entry.pre_tbl     <- sample_source entry.word_ug.pre entry.pre_ed entry.pos Pre;
      entry.stm_tbl     <- sample_source entry.word_ug.stm entry.stm_ed entry.pos Stm;
      entry.suf_tbl     <- sample_source entry.word_ug.suf entry.suf_ed entry.pos Suf;
               
      increment_pos_counts entry;
      increment_mrf_counts entry Pre i;
      increment_mrf_counts entry Stm i;
      increment_mrf_counts entry Suf i;
   end;
   check_edit_counts i
;;

let best_init_prediction e i = 
   if A.length e.all_possible = 0 then begin
      printf p"no possible: %d" i;
      e.xxx <- true;
      e.cog <- false;
      e.best_cog <- false;
   end
   else begin
      if not !init then begin
         let cog_prob = e.all_cog_prob.(0) *. cog_prior in
         let noncog_prob = e.all_cog_prob.(1) *. (1. -. cog_prior) in
         e.best_cog <- (cog_prob > noncog_prob);
      end;

      let base, true_count = true,true in
      let probs =
         A.map (begin fun p -> 
            let ug_pre = p.p_ug.pre in
            let ug_stm = p.p_ug.stm in
            let ug_suf = p.p_ug.suf in
            let hb_pre = p.p_hb.pre in
            let hb_stm = p.p_hb.stm in
            let hb_suf = p.p_hb.suf in
            let pre_ed = p.p_pre_ed in
            let stm_ed = p.p_stm_ed in
            let suf_ed = p.p_suf_ed in
            let pos = p.p_pos in
            morph_prob ~base ~true_count ug_pre hb_pre pre_ed pos Pre *. 
            morph_prob ~base ~true_count ug_stm hb_stm stm_ed pos Stm *. 
            morph_prob ~base ~true_count ug_suf hb_suf suf_ed pos Suf
         end) e.all_possible in

      let resi, best_prob = max_itemi probs in
      e.all_probs.(resi) <- e.all_probs.(resi) +. 1.;
      let best = e.all_possible.(resi) in
      e.best_prob   <- best_prob;
      e.best_ug     <- best.p_ug;
      e.best_hb.pre <- best.p_hb.pre;
      e.best_hb.stm <- best.p_hb.stm;
      e.best_hb.suf <- best.p_hb.suf;
      e.best_pos    <- best.p_pos;

      L.iter (Ht.incr' d.best_edit_count) best.p_pre_ed;
      L.iter (Ht.incr' d.best_edit_count) best.p_stm_ed;
      L.iter (Ht.incr' d.best_edit_count) best.p_suf_ed;
      
      let mrf_count = d.best_mrf_count2 in
      
      let pre_key = (e.best_ug.pre, e.best_pos, Pre) in
      let stm_key = (e.best_ug.stm, e.best_pos, Stm) in
      let suf_key = (e.best_ug.suf, e.best_pos, Suf) in
      
      if not (Ht.mem mrf_count pre_key) then
         Ht.add mrf_count pre_key (Ht.create 50);
      if not (Ht.mem mrf_count stm_key) then
         Ht.add mrf_count stm_key (Ht.create 50);
      if not (Ht.mem mrf_count suf_key) then
         Ht.add mrf_count suf_key (Ht.create 50);
      
      Ht.incr' (Ht.find mrf_count pre_key) best.p_pre_ed;
      Ht.incr' (Ht.find mrf_count stm_key) best.p_stm_ed;
      Ht.incr' (Ht.find mrf_count suf_key) best.p_suf_ed;
            
      if !init then begin
         e.word_ug      <- best.p_ug;
         e.word_hb.pre  <- best.p_hb.pre;
         e.word_hb.stm  <- best.p_hb.stm;
         e.word_hb.suf  <- best.p_hb.suf;
         e.pos          <- best.p_pos;
         e.pre_ed       <- best.p_pre_ed;
         e.stm_ed       <- best.p_stm_ed;
         e.suf_ed       <- best.p_suf_ed;

         let get_tbl mtype = 
            let mrf     = match mtype with | Pre -> e.word_ug.pre    | Stm -> e.word_ug.stm  | Suf -> e.word_ug.suf in
            let ed      = match mtype with | Pre -> e.pre_ed         | Stm -> e.stm_ed       | Suf -> e.suf_ed in
            let tbls    = d.tbls in
            let pos     = e.pos in
            let key     = (mrf,pos,mtype) in

            if not (Ht.mem tbls key) then Hs.create 50
            else (
               let ht = Ht.find tbls key in
               if not (Ht.mem ht ed) then Hs.create 50
               else (
                  let da = Ht.find ht ed in
                  assert (Da.length da = 1);
                  Da.get da 0
               )
            ) 
         in         
         e.pre_tbl <- get_tbl Pre;
         e.stm_tbl <- get_tbl Stm;
         e.suf_tbl <- get_tbl Suf;
      end;
      (* printf p"%{float array} %d" probs (count_predictions e i); *)
      if best_prob = 0. then begin
         e.xxx <- true;
         e.cog <- false;
         e.best_cog <- false;
         decr cogs
      end;

      if e.cog then begin
         increment_pos_counts e;
         increment_mrf_counts ~best:true e Pre i;
         increment_mrf_counts ~best:true e Stm i;
         increment_mrf_counts ~best:true e Suf i
      end
   end;
   check_edit_counts i
;;

let sample_morphs () =
   check_edit_counts (-9999);
   A.switeri d.entries (fun i entry ->
      (* if i mod 100 = 0 then
         printf p"sampling morph  %d" i; *)
      if not entry.xxx then
         sample_prediction entry i
   );
   (* resample each table from the base distribution *)
   (* printf p"resampling morphs from base distr..."; *)
   let resample ht tbl mrf oedits pos mtype =
      assert (Hs.length tbl > 0);
      assert (L.length oedits > 0);

      let mrf_counts = get_morph_counts mrf mtype pos in
      let n = foi (Hs.length tbl) in
      let otbls = Ht.find ht oedits in
      Da.remove otbls tbl;
      Ht.add_float mrf_counts oedits (-.n);
      let decr_ed ed =
         Ht.decr' d.edit_count ed;
         Ht.add_float d.true_edit_count ed (-.n);
         match ed with
            | Sub _ | End -> d.sub_sum <- d.sub_sum -. 1.; d.true_sub_sum <- d.true_sub_sum -. n
            | Ins _ -> d.ins_sum <- d.ins_sum -. 1.; d.true_ins_sum <- d.true_ins_sum -. n
            | Del _ -> d.del_sum <- d.del_sum -. 1.; d.true_del_sum <- d.true_del_sum -. n
      in
      L.iter decr_ed oedits;

      let ohb_mrf = hb_from_edits oedits in
      Hs.iter (fun i -> 
         let entry = d.entries.(i) in
         let ohb_mrf' = match mtype with Pre -> entry.word_hb.pre | Stm -> entry.word_hb.stm | Suf -> entry.word_hb.suf in
         let oedits' = match mtype with Pre -> entry.pre_ed | Stm -> entry.stm_ed | Suf -> entry.suf_ed in
         let ohb_mrf'' = hb_from_edits oedits' in
         assert (oedits = oedits');
         assert (ohb_mrf' = ohb_mrf'');
         assert (ohb_mrf = ohb_mrf')
      ) tbl;
      
      if mtype != Stm && A.length ohb_mrf >= 0 then begin
         assert (Ht.find d.pos_affix_sum (pos, mtype) >= n);
         assert (Ht.find d.pos_affix_count (ohb_mrf, pos, mtype) >= n);
         Ht.add_float d.pos_affix_sum (pos, mtype) (-.n);
         Ht.add_float d.pos_affix_count (ohb_mrf, pos, mtype) (-.n)
      end;

      (* if ohb_mrf = d.encode_hb "w" && mtype = Suf && pos = "v" then
         printf p"(1) regular mrf_count = %f  <? affix count = %f + %f" (Ht.count' mrf_counts oedits) (Ht.count' d.pos_affix_count (ohb_mrf, pos, mtype)) n; *)

      let poss = get_possible_edits mrf pos mtype in
      assert (n > 0.);
      let probs = A.map (fun el -> let hb_mrf = hb_from_edits el in morph_prob ~num:n ~base:true mrf hb_mrf el pos mtype) poss in
      let idx = sample probs in
      let nedits = poss.(idx) in
      let nhb_mrf = hb_from_edits nedits in
      
      Hs.iter (fun i ->
         let entry = d.entries.(i) in
         match mtype with 
            | Pre -> entry.pre_ed <- nedits; entry.word_hb.pre <- nhb_mrf
            | Stm -> entry.stm_ed <- nedits; entry.word_hb.stm <- nhb_mrf
            | Suf -> entry.suf_ed <- nedits; entry.word_hb.suf <- nhb_mrf
      ) tbl;
      Ht.add_float mrf_counts nedits n;
      let incr_ed ed =
         Ht.incr' d.edit_count ed;
         Ht.add_float d.true_edit_count ed n;
         match ed with
            | Sub _ | End -> d.sub_sum <- d.sub_sum +. 1.; d.true_sub_sum <- d.true_sub_sum +. n
            | Ins _ -> d.ins_sum <- d.ins_sum +. 1.; d.true_ins_sum <- d.true_ins_sum +. n
            | Del _ -> d.del_sum <- d.del_sum +. 1.; d.true_del_sum <- d.true_del_sum +. n
      in
      L.iter incr_ed nedits;

      (* if nhb_mrf = d.encode_hb "w" && mtype = Suf && pos = "v" then
         printf p"(2) regular mrf_count = %f  <? affix count = %f + %f" (Ht.count' mrf_counts nedits) (Ht.count' d.pos_affix_count (nhb_mrf, pos, mtype)) n; *)
      
      if mtype != Stm && A.length nhb_mrf >= 0 then begin
         Ht.add_float d.pos_affix_sum (pos, mtype) n;
         Ht.add_float d.pos_affix_count (nhb_mrf, pos, mtype) n
      end;

      if not (Ht.mem ht nedits) then Ht.add ht nedits (Da.make 50);
      let ntbls = Ht.find ht nedits in
      Da.add ntbls tbl;
   in

   Ht.iter (fun (mrf,pos,mtype) ht ->
    Ht.iter (fun edits da ->
       A.iter (fun tbl -> resample ht tbl mrf edits pos mtype) (Da.to_array da)
    ) ht
   ) d.tbls;
   
   (* add base distr edit counts to edits_record *)
   Ht.iter (fun e n -> Ht.add_float edits_record e n) d.edit_count
;;

let get_log_prob () =
   let gp = gammas_prior () in
   let gl = gammas_ll () in
   let res = ref (gp +. gl) in
      
   let stm_denom = asc_fact alpha_stm d.n in
   let pre_denom = A.fold_left (fun acc pos -> let n = Ht.count' d.pos_count pos in acc +. asc_fact alpha_pre n) 0. d.poses in
   let suf_denom = A.fold_left (fun acc pos -> let n = Ht.count' d.pos_count pos in acc +. asc_fact alpha_suf n) 0. d.poses in

   res := !res -. stm_denom -. pre_denom -. suf_denom;
   
   Ht.iter (fun (mrf, pos, mtype) ht ->
      Ht.iter (fun el da ->
         let ins_pen = log (if L.exists (function Ins _ -> true | _ -> false) el then ins_prior /. foi (A.length mrf + 1) else (1. -. ins_prior)) in
         let del_pen = log (if L.exists (function Del _ -> true | _ -> false) el then del_prior /. foi (A.length mrf) else (1. -. del_prior)) in
         let alpha   = log (match mtype with Pre -> alpha_pre | Stm -> alpha_stm | Suf -> alpha_suf) in
         let numer1  = ins_pen +. del_pen +. alpha in
         Da.iter (fun tbl -> let k = Hs.length tbl in
            if k > 0 then res := !res +. numer1;
            if k > 2 then let numer2 = log_fact_approx (k - 1) in res := !res +. numer2
         ) da
      ) ht
   ) d.tbls;
   
   A.iter (fun e ->
      if e.cog then 
         res := !res +. log cog_prior
      else begin
         res := !res +. log (1. -. cog_prior) +. get_noncog_logprob e.word_ug
      end
   ) d.entries;
   !res
;;

let write_output () =
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/ugaritic_" ^ soi !r ^ ".out") 
      (fun out -> Array.print ~first:"" ~last:"\n" ~sep:"\n" entry_print out d.entries)
;;
let write_best () =
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/best_ugaritic_" ^ soi !r ^ ".out") 
      (fun out -> Array.print ~first:"" ~last:"\n" ~sep:"\n" entry_print_best out d.entries)
;;
let write_morph_counts () =
   let f ht mtype out = 
      let res = Ht.create 20 in
      Ht.iter (fun (morph,pos,mtype_) ht2 ->
         if mtype = mtype_ then
            Ht.iter (fun el c ->
               let ug_str = d.decode_ug morph in
               let hb_str = d.decode_hb (hb_from_edits el) in
               Ht.add_int res (ug_str,hb_str,pos) (iof c)
            ) ht2
      ) ht;
      let a = Ht.to_array (Ht.filter (fun c -> c > 0) res) in
      A.sort (fun x y -> - compare (snd x) (snd y)) a;
      Array.print ~first:"" ~last:"\n" ~sep:"\n"
         (fun out ((ug_str,hb_str,pos), c) -> fprintf out p"%s %s (%s)\t%15d" ug_str hb_str pos c)
         out a
   in
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/pre_" ^ soi !r ^ ".counts") (f d.mrf_count Pre);
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/stm_" ^ soi !r ^ ".counts") (f d.mrf_count Stm);
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/suf_" ^ soi !r ^ ".counts") (f d.mrf_count Suf);
;;
let write_best_morph_counts () =
   let f ht mtype out = 
      let res = Ht.create 20 in
      Ht.iter (fun (morph,pos,mtype_) ht2 ->
         if mtype = mtype_ then
            Ht.iter (fun el c ->
               let ug_str = d.decode_ug morph in
               let hb_str = d.decode_hb (hb_from_edits el) in
               Ht.add_int res (ug_str,hb_str,pos) (iof c)
            ) ht2
      ) ht;
      let a = Ht.to_array (Ht.filter (fun c -> c > 0) res) in
      A.sort (fun x y -> - compare (snd x) (snd y)) a;
      Array.print ~first:"" ~last:"\n" ~sep:"\n"
         (fun out ((ug_str,hb_str,pos), c) -> fprintf out p"%s %s (%s)\t%15d" ug_str hb_str pos c)
         out a
   in
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/best_pre_" ^ soi !r ^ ".counts") (f d.best_mrf_count2 Pre);
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/best_stm_" ^ soi !r ^ ".counts") (f d.best_mrf_count2 Stm);
   File.with_file_out ~mode:[`trunc;`text;`create] (output_dir ^ "/best_suf_" ^ soi !r ^ ".counts") (f d.best_mrf_count2 Suf);
;;

let set_to_best () =
   A.switeri d.entries (fun i entry ->
      if not entry.xxx then begin
         if entry.cog then decrement_entry_counts entry i;
         
         entry.word_ug <- entry.best_ug;
         entry.word_hb.pre <- entry.best_hb.pre;
         entry.word_hb.stm <- entry.best_hb.stm;
         entry.word_hb.suf <- entry.best_hb.suf;
         entry.pos <- entry.best_pos;
         entry.cog <- entry.best_cog;

         if entry.cog then begin
            increment_pos_counts entry;
            increment_mrf_counts entry Pre i;
            increment_mrf_counts entry Stm i;
            increment_mrf_counts entry Suf i
         end
      end
   )
;;  
let init_entries () =   
   let pre_er = Ht.create 500 in
   let stm_er = Ht.create 500 in
   let suf_er = Ht.create 500 in
   let pre_erb = Ht.create 500 in
   let stm_erb = Ht.create 500 in
   let suf_erb = Ht.create 500 in

   let cog_tp        = ref 0 in
   let cog_fp        = ref 0 in
   let cog_true      = ref 0 in
    
   let perfectb      = ref 0 in
   let perfect       = ref 0 in
   let dist_totalb   = ref 0 in
   let dist_total    = ref 0 in
   let perfectb_tok      = ref 0 in
   let perfect_tok       = ref 0 in
   let dist_totalb_tok   = ref 0 in
   let dist_total_tok    = ref 0 in
   
   let perfect_wb    = ref 0 in
   let perfect_w     = ref 0 in
   let dist_total_wb = ref 0 in
   let dist_total_w  = ref 0 in

   let perfect_wb_tok    = ref 0 in
   let perfect_w_tok     = ref 0 in
   let dist_total_wb_tok = ref 0 in
   let dist_total_w_tok  = ref 0 in


   Ht.clear d.best_edit_count;
   Ht.clear d.best_mrf_count2;
   A.switeri d.entries (fun i entry ->
      if i mod 1 = 100 then 
         printf p"eval: %d" i;

      if not entry.xxx then
         best_init_prediction entry i;
      
      if not entry.true_cog then
         (if entry.best_cog then incr cog_fp)
      
      else begin   
         incr cog_true;
         if entry.best_cog then incr cog_tp;
         
         let ug_preb = d.decode_ug entry.best_ug.pre in
         let ug_stmb = d.decode_ug entry.best_ug.stm in
         let ug_sufb = d.decode_ug entry.best_ug.suf in
         let ug_pre = d.decode_ug entry.word_ug.pre in
         let ug_stm = d.decode_ug entry.word_ug.stm in
         let ug_suf = d.decode_ug entry.word_ug.suf in

         let pre_prdb = d.decode_hb entry.best_hb.pre in
         let stm_prdb = d.decode_hb entry.best_hb.stm in
         let suf_prdb = d.decode_hb entry.best_hb.suf in
         let wrd_prdb = pre_prdb ^ stm_prdb ^ suf_prdb in

         let pre_prd = d.decode_hb entry.word_hb.pre in
         let stm_prd = d.decode_hb entry.word_hb.stm in
         let suf_prd = d.decode_hb entry.word_hb.suf in
         let wrd_prd = pre_prd ^ stm_prd ^ suf_prd in

         let pres = L.map (fun w -> d.decode_hb w.pre) entry.true_words_hb in
         let stms = L.map (fun w -> d.decode_hb w.stm) entry.true_words_hb in
         let sufs = L.map (fun w -> d.decode_hb w.suf) entry.true_words_hb in
         let wrds = L.map (fun w -> d.decode_hb w.pre ^ d.decode_hb w.stm ^ d.decode_hb w.suf) entry.true_words_hb in

         let pre_distsb = L.map (edit_dist pre_prdb) pres in
         let stm_distsb = L.map (edit_dist stm_prdb) stms in
         let suf_distsb = L.map (edit_dist suf_prdb) sufs in
         let wrd_distsb = L.map (edit_dist wrd_prdb) wrds in
         let pre_dists = L.map (edit_dist pre_prd) pres in
         let stm_dists = L.map (edit_dist stm_prd) stms in
         let suf_dists = L.map (edit_dist suf_prd) sufs in
         let wrd_dists = L.map (edit_dist wrd_prd) wrds in

         let preib, pre_distb = L.min_itemi pre_distsb in
         let stmib, stm_distb = L.min_itemi stm_distsb in
         let sufib, suf_distb = L.min_itemi suf_distsb in
         let wrd_distb = L.min wrd_distsb in

         let prei, pre_dist = L.min_itemi pre_dists in
         let stmi, stm_dist = L.min_itemi stm_dists in
         let sufi, suf_dist = L.min_itemi suf_dists in
         let wrd_dist = L.min wrd_dists in

         let n = entry.count in
         
         if pre_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr pre_er (ug_pre, pre_prd, L.nth pres prei);
         if stm_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr stm_er (ug_stm, stm_prd, L.nth stms stmi);
         if suf_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr suf_er (ug_suf, suf_prd, L.nth sufs sufi);
         if wrd_dist = 0 then (incr perfect_w; perfect_w_tok += n);
         dist_total     := !dist_total + pre_dist + stm_dist + suf_dist;
         dist_total_w   := !dist_total_w + wrd_dist;
         dist_total_tok     := !dist_total + (pre_dist + stm_dist + suf_dist) * n;
         dist_total_w_tok   := !dist_total_w + wrd_dist * n;

         if pre_distb = 0 then (incr perfectb; perfectb_tok += n) else Ht.incr pre_erb (ug_preb, pre_prdb, L.nth pres preib);
         if stm_distb = 0 then (incr perfectb; perfectb_tok += n) else Ht.incr stm_erb (ug_stmb, stm_prdb, L.nth stms stmib);
         if suf_distb = 0 then (incr perfectb; perfectb_tok += n) else Ht.incr suf_erb (ug_sufb, suf_prdb, L.nth sufs sufib);
         if wrd_distb = 0 then (incr perfect_wb; perfect_wb_tok += n);
         dist_totalb    := !dist_totalb + pre_distb + stm_distb + suf_distb;
         dist_total_wb  := !dist_total_wb + wrd_distb;
         dist_totalb_tok     := !dist_totalb + (pre_distb + stm_distb + suf_distb) * n;
         dist_total_wb_tok   := !dist_total_wb + wrd_distb * n;
      end
   );
   let n_w = A.count (fun e -> e.true_cog) d.entries in
   let tk_n_w = A.fold_left (fun acc e -> acc + (if e.true_cog then e.count else 0)) 0 d.entries in
   let tk_n = tk_n_w * 3 in
   let n = n_w * 3 in
   printf p"perfect mrfs  = %f (%f)   %f (%f)" (foi !perfectb /. foi n) (foi !perfectb_tok /. foi tk_n) (foi !perfect /. foi n) (foi !perfect_tok /. foi tk_n);
   printf p"avg edit dist = %f (%f)   %f (%f)" (foi !dist_totalb /. foi n) (foi !dist_totalb_tok /. foi tk_n) (foi !dist_total /. foi n) (foi !dist_total_tok /. foi tk_n);
   printf p"perfect wrds  = %f (%f)   %f (%f)" (foi !perfect_wb /. foi n_w) (foi !perfect_wb_tok /. foi tk_n_w) (foi !perfect_w /. foi n_w) (foi !perfect_w_tok /. foi tk_n_w);
   printf p"avg edit dist = %f (%f)   %f (%f)" (foi !dist_total_wb /. foi n_w) (foi !dist_total_wb_tok /. foi tk_n_w) (foi !dist_total_w /. foi n_w) (foi !dist_total_w_tok /. foi tk_n_w);
   printf p"cog prediction:  pr = %f, re = %f" (foi !cog_tp /. foi (!cog_tp + !cog_fp)) (foi !cog_tp /. foi !cog_true); 
   let lp = if !init then 1.0 else get_log_prob () in
   printf p"log-prob = %f" lp;
   
   let write fn ht =
      File.with_file_out ~mode:[`trunc;`text;`create] fn
      (fun out -> 
         let a = Ht.to_array ht in
         A.sort (fun x y ->  - compare (snd x) (snd y)) a;
         Array.print ~first:"" ~last:"\n" ~sep:"\n" (tuple2_print ~first:"" ~last:"" ~sep:"\t\t" (tuple3_print S.print S.print S.print) Int.print) out a)
   in
   if !r < 50 || !r mod 20 = 2 then begin
      write (output_dir ^ "/best_pre_" ^ soi !r ^".errors") pre_erb;
      write (output_dir ^ "/best_stm_" ^ soi !r ^".errors") stm_erb;
      write (output_dir ^ "/best_suf_" ^ soi !r ^".errors") suf_erb;
      write (output_dir ^ "/pre_" ^ soi !r ^".errors") pre_er;
      write (output_dir ^ "/stm_" ^ soi !r ^".errors") stm_er;
      write (output_dir ^ "/suf_" ^ soi !r ^".errors") suf_er;
   end
;;

let prepare_best ~only_gammas () =
   let best_gammas = Ht.create 100 in
   let best_sub_list, n = Ht.max_item gammas_record in
   assert (n >= 0);
   L.iter (fun ed -> Ht.add best_gammas ed true) best_sub_list;
   let sub_sum = ref 0. in
   let ins_sum = ref 0. in
   let del_sum = ref 0. in
   let alphas_sub_sum = ref gamma_sub_false in (* for End *)
   let alphas_ins_sum = ref d.alphas_ins_sum in  (* XXX assumes these two are constant!!! *)
   let alphas_del_sum = ref d.alphas_del_sum in  (* XXX assumes these two are constant!!! *)
   foreach (L.enum d.edits) (fun ed -> 
      let n = Ht.count' edits_record ed /. foi (!r - 1) in
      match ed with
         | Sub (u,h) -> (match only_gammas with
            | true ->
               if Ht.mem best_gammas ed then (
                  sub_sum +.= n;
                  alphas_sub_sum +.= gamma_sub_false  (* no reason to use gamma_sub_true when only gammas are allowed *)
               )
            | false ->
               sub_sum +.= n;
               if L.mem h d.mapping1.(u) then
                  alphas_sub_sum +.= (if Ht.mem best_gammas ed then gamma_sub_true else gamma_sub_false))
         | Ins _ ->  ins_sum +.= n
         | Del _ ->  del_sum +.= n
         | End ->    sub_sum +.= n
   );
   foreach (L.enum d.edits) (fun ed ->
      let n, alpha, alpha_sum, n_sum = match ed with 
         | Sub _ -> 
            (if only_gammas && not (Ht.mem best_gammas ed) then 0. else Ht.count' edits_record ed /. foi (!r - 1)),
            (if only_gammas then (if Ht.mem best_gammas ed then gamma_sub_false else 0.) else (if Ht.mem best_gammas ed then gamma_sub_true else gamma_sub_false)),
            !alphas_sub_sum, !sub_sum
         | End    -> Ht.count' edits_record ed /. foi (!r - 1), gamma_sub_false, !alphas_sub_sum, !sub_sum 
         | Ins _  -> Ht.count' edits_record ed /. foi (!r - 1), gamma_ins_false, !alphas_ins_sum, !ins_sum
         | Del _  -> Ht.count' edits_record ed /. foi (!r - 1), gamma_del_false, !alphas_del_sum, !del_sum in
      Ht.replace best_edit_probs ed ((alpha +. n) /. (alpha_sum +. n_sum))
   )
;;
let get_best_possible ug = 
   let probs = A.make (A.length d.poses) 0. in
   let bests = A.make (A.length d.poses) ([],[],[]) in
   A.iteri (fun i pos ->
      let pres = get_possible_edits ug.pre pos Pre in
      let stms = get_possible_edits ug.stm pos Stm in
      let sufs = get_possible_edits ug.suf pos Suf in
      if not (A.length pres = 0 || A.length stms = 0 || A.length sufs = 0) then begin
         let get_prob el = L.fold_left (fun acc ed -> acc *. Ht.find best_edit_probs ed) 1. el in
         let pre_probs = A.map get_prob pres in
         let stm_probs = A.map get_prob stms in
         let suf_probs = A.map get_prob sufs in
         let pre_i, pre_p = max_itemi pre_probs in
         let stm_i, stm_p = max_itemi stm_probs in
         let suf_i, suf_p = max_itemi suf_probs in
         probs.(i) <- pre_p *. stm_p *. suf_p;
         (* printf p"prob (%s) = %f" d.poses.(i) probs.(i); *)
         bests.(i) <- (pres.(pre_i), stms.(stm_i), sufs.(suf_i))
      end
   ) d.poses;
   if A.for_all (fun v -> v = 0.) probs then raise (Failure "no possible");
   let idx, mx_prob = max_itemi probs in
   let pre_ed, stm_ed, suf_ed = bests.(idx) in 
   {  p_ug = ug; p_hb = {pre = hb_from_edits pre_ed; stm = hb_from_edits stm_ed; suf = hb_from_edits suf_ed}; 
      p_pre_ed = pre_ed; 
      p_stm_ed = stm_ed; 
      p_suf_ed = suf_ed; 
      p_pos = d.poses.(idx) }
;;

let get_very_best_possible all_possible =
   let probs = A.map (fun poss ->
      let get_prob el = L.fold_left (fun acc ed -> acc *. Ht.find best_edit_probs ed) 1. el in
      let pre_p = get_prob poss.p_pre_ed in
      let stm_p = get_prob poss.p_stm_ed in
      let suf_p = get_prob poss.p_suf_ed in
      pre_p *. stm_p *. suf_p
   ) all_possible in
   if A.for_all (fun v -> v = 0.) probs then raise (Failure "no possible");
   let idx, mx_prob = max_itemi probs in
   all_possible.(idx)
;;

let get_best_possible_subs ug_mrf =
   let get_best_ed u =
      let probs = aol $ L.map (fun h -> Ht.find best_edit_probs (Sub (u,h))) d.mapping1.(u) in
      let idx, _ = max_itemi probs in
      Sub (u, L.nth d.mapping1.(u) idx)
   in
   let eds = loa (A.map get_best_ed ug_mrf) @ [End] in
   let hb_mrf = hb_from_edits eds in
   (hb_mrf, eds)
;;

let eval () =
   prepare_best ~only_gammas:only_gammas ();
   let pre_er = Ht.create 500 in
   let stm_er = Ht.create 500 in
   let suf_er = Ht.create 500 in

   let cog_tp        = ref 0 in
   let cog_fp        = ref 0 in
   let cog_true      = ref 0 in

   let perfect      = ref 0 in
   let dist_total   = ref 0 in
   let perfect_tok       = ref 0 in
   let dist_total_tok   = ref 0 in

   let perfect_w     = ref 0 in
   let dist_total_w  = ref 0 in   
   let perfect_w_tok     = ref 0 in
   let dist_total_w_tok  = ref 0 in
   
   Ht.clear d.best_edit_count;
   Ht.clear d.best_mrf_count2;
   A.switeri d.entries (fun i e ->
      if i mod 1 = 100 then 
         printf p"eval: %d" i;

      let b = if A.length e.all_possible = 0 then
         let pre, pre_ed = get_best_possible_subs e.word_ug.pre in
         let stm, stm_ed = get_best_possible_subs e.word_ug.stm in
         let suf, suf_ed = get_best_possible_subs e.word_ug.suf in
         {p_ug = e.word_ug; p_hb = {pre=pre;stm=stm;suf=suf}; p_pre_ed=pre_ed; p_stm_ed=stm_ed; p_suf_ed=suf_ed; p_pos = "xxx"}
      else 
         (try 
            (try 
               let word_ug, n = Ht.max_item e.segments_record in
               assert (n > 0);
               get_best_possible word_ug
            with 
               | Not_found -> 
                  get_very_best_possible e.all_possible
               | Failure "no possible" -> 
                  e.best_cog <- false;
                  get_very_best_possible e.all_possible)
         with Failure "no possible" ->
            e.best_cog <- false;
            let pre, pre_ed = get_best_possible_subs e.word_ug.pre in
            let stm, stm_ed = get_best_possible_subs e.word_ug.stm in
            let suf, suf_ed = get_best_possible_subs e.word_ug.suf in
            {p_ug = e.word_ug; p_hb = {pre=pre;stm=stm;suf=suf}; p_pre_ed=pre_ed; p_stm_ed=stm_ed; p_suf_ed=suf_ed; p_pos = "-"})
      in

      e.best_ug     <- b.p_ug;
      e.best_hb.pre <- b.p_hb.pre;
      e.best_hb.stm <- b.p_hb.stm;
      e.best_hb.suf <- b.p_hb.suf;
      e.best_pos    <- b.p_pos;
      
      if not e.true_cog then begin
         if e.best_cog then incr cog_fp;
         
         if make_all_cog then begin
            L.iter (Ht.incr' d.best_edit_count) b.p_pre_ed;
            L.iter (Ht.incr' d.best_edit_count) b.p_stm_ed;
            L.iter (Ht.incr' d.best_edit_count) b.p_suf_ed;

            let mrf_count = d.best_mrf_count2 in
      
            let pre_key = (b.p_ug.pre, b.p_pos, Pre) in
            let stm_key = (b.p_ug.stm, b.p_pos, Stm) in
            let suf_key = (b.p_ug.suf, b.p_pos, Suf) in
      
            if not (Ht.mem mrf_count pre_key) then
               Ht.add mrf_count pre_key (Ht.create 50);
            if not (Ht.mem mrf_count stm_key) then
               Ht.add mrf_count stm_key (Ht.create 50);
            if not (Ht.mem mrf_count suf_key) then
               Ht.add mrf_count suf_key (Ht.create 50);
      
            Ht.incr' (Ht.find mrf_count pre_key) b.p_pre_ed;
            Ht.incr' (Ht.find mrf_count stm_key) b.p_stm_ed;
            Ht.incr' (Ht.find mrf_count suf_key) b.p_suf_ed;
         end
      end
      
      else begin
         incr cog_true;
         if e.best_cog then incr cog_tp;

         L.iter (Ht.incr' d.best_edit_count) b.p_pre_ed;
         L.iter (Ht.incr' d.best_edit_count) b.p_stm_ed;
         L.iter (Ht.incr' d.best_edit_count) b.p_suf_ed;

         let mrf_count = d.best_mrf_count2 in
      
         let pre_key = (b.p_ug.pre, b.p_pos, Pre) in
         let stm_key = (b.p_ug.stm, b.p_pos, Stm) in
         let suf_key = (b.p_ug.suf, b.p_pos, Suf) in
      
         if not (Ht.mem mrf_count pre_key) then
            Ht.add mrf_count pre_key (Ht.create 50);
         if not (Ht.mem mrf_count stm_key) then
            Ht.add mrf_count stm_key (Ht.create 50);
         if not (Ht.mem mrf_count suf_key) then
            Ht.add mrf_count suf_key (Ht.create 50);
      
         Ht.incr' (Ht.find mrf_count pre_key) b.p_pre_ed;
         Ht.incr' (Ht.find mrf_count stm_key) b.p_stm_ed;
         Ht.incr' (Ht.find mrf_count suf_key) b.p_suf_ed;

         let ug_pre = d.decode_ug b.p_ug.pre in
         let ug_stm = d.decode_ug b.p_ug.stm in
         let ug_suf = d.decode_ug b.p_ug.suf in

         let pre_prd = d.decode_hb b.p_hb.pre in
         let stm_prd = d.decode_hb b.p_hb.stm in
         let suf_prd = d.decode_hb b.p_hb.suf in
         let wrd_prd = pre_prd ^ stm_prd ^ suf_prd in

         let pres = L.map (fun w -> d.decode_hb w.pre) e.true_words_hb in
         let stms = L.map (fun w -> d.decode_hb w.stm) e.true_words_hb in
         let sufs = L.map (fun w -> d.decode_hb w.suf) e.true_words_hb in
         let wrds = L.map (fun w -> d.decode_hb w.pre ^ d.decode_hb w.stm ^ d.decode_hb w.suf) e.true_words_hb in

         let pre_dists = L.map (edit_dist pre_prd) pres in
         let stm_dists = L.map (edit_dist stm_prd) stms in
         let suf_dists = L.map (edit_dist suf_prd) sufs in
         let wrd_dists = L.map (edit_dist wrd_prd) wrds in

         let prei, pre_dist = L.min_itemi pre_dists in
         let stmi, stm_dist = L.min_itemi stm_dists in
         let sufi, suf_dist = L.min_itemi suf_dists in
         let wrd_dist = L.min wrd_dists in
         
         let n = e.count in
         
         if pre_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr pre_er (ug_pre, pre_prd, L.nth pres prei);
         if stm_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr stm_er (ug_stm, stm_prd, L.nth stms stmi);
         if suf_dist = 0 then (incr perfect; perfect_tok += n) else Ht.incr suf_er (ug_suf, suf_prd, L.nth sufs sufi);
         if wrd_dist = 0 then (incr perfect_w; perfect_w_tok += n);
         dist_total     := !dist_total + pre_dist + stm_dist + suf_dist;
         dist_total_w   := !dist_total_w + wrd_dist;
         dist_total_tok     := !dist_total_tok + (pre_dist + stm_dist + suf_dist) * n;
         dist_total_w_tok   := !dist_total_w_tok + wrd_dist * n;

      end
   );
   let n_w = A.count (fun e -> e.true_cog) d.entries in
   let n = n_w * 3 in
   let tok_n_w = A.fold_left (fun acc e -> acc + (if e.true_cog then e.count else 0)) 0 d.entries in
   let tok_n = tok_n_w * 3 in
   printf p"eval========================================";
   printf p"perfect mrfs  = %f (%f)" (foi !perfect /. foi n) (foi !perfect_tok /. foi tok_n);
   printf p"avg edit dist = %f (%f)" (foi !dist_total /. foi n) (foi !dist_total_tok /. foi tok_n);
   printf p"perfect wrds  = %f (%f)" (foi !perfect_w /. foi n_w) (foi !perfect_w_tok /. foi tok_n_w);
   printf p"avg edit dist = %f (%f)" (foi !dist_total_w /. foi n_w) (foi !dist_total_w_tok /. foi tok_n_w);
   printf p"cog prediction:  pr = %f, re = %f" (foi !cog_tp /. foi (!cog_tp + !cog_fp)) (foi !cog_tp /. foi !cog_true); 
   let lp = if !init then 1.0 else get_log_prob () in
   printf p"log-prob = %f" lp;

   let write fn ht =
      File.with_file_out ~mode:[`trunc;`text;`create] fn
      (fun out -> 
         let a = Ht.to_array ht in
         A.sort (fun x y ->  - compare (snd x) (snd y)) a;
         Array.print ~first:"" ~last:"\n" ~sep:"\n" (tuple2_print ~first:"" ~last:"" ~sep:"\t\t" (tuple3_print S.print S.print S.print) Int.print) out a)
   in
   if !r < 50 || !r mod 20 = 2 then begin
      write (output_dir ^ "/best2_pre_" ^ soi !r ^".errors") pre_er;
      write (output_dir ^ "/best2_stm_" ^ soi !r ^".errors") stm_er;
      write (output_dir ^ "/best2_suf_" ^ soi !r ^".errors") suf_er;
   end
;;
let main () =
   cogs := A.length d.entries;
   
   if one_step then
      init_gammas1_debug ()
   else begin
      A.iteri all_possible_predictions d.entries;
      init_entries ();
      write_output ();
      write_gammas ~best:false ();
      write_morph_counts ();
      init_gammas1 ();
   end;
   
   incr r;
   write_gammas ~best:false ();
   incr r;
   A.iteri all_possible_predictions d.entries;   
   init_entries ();
   init_gammas2 ();
   write_output ();
   write_gammas ~best:false ();
   write_morph_counts ();

   init := false;

   cogs := A.fold_left (fun acc e -> if e.xxx then acc else acc + 1) 0 d.entries;

   while !r < rounds do
      incr r;
      printf  p"SAMPLING round = %d (%d cogs)" (!r) !cogs;
      sample_morphs ();
      sample_gammas ();
      
      if !r mod 5 = 2 then begin
         printf p"evaluating...";
         eval ();
         
         if !r < 50 || !r mod 20 = 2 then begin
            write_best ();
            write_gammas ~best:true ();
            write_best_morph_counts ();
            write_output ();
            write_gammas ~best:false ();
            write_morph_counts ();
         end
      end;
   done
;;







let _ = Callback.register "edit_prob" edit_prob_c;;
if not !Sys.interactive then main ()
