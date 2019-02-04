(*pp $PP *)

(** Various utility types and functions of my own devising *)
 
open Batteries, Batteries.Standard, Batteries.Print

(* module Hashtbl = ExtHashtbl.Hashtbl
module Array = ExtArray.Array
module String = ExtString.String
module List = ExtList.List *)
module Opt = Option
module Dray = MyDynArray

open Std
(* open Num *)
open Printf
open CamomileLibrary.Default.Camomile

let ( += ) (r : int ref) (n : int) = r := !r + n
let ( -= ) (r : int ref) (n : int) = r := !r - n
let ( *= ) (r : int ref) (n : int) = r := !r * n
let ( /= ) (r : int ref) (n : int) = r := !r / n

let ( +.= ) (r : float ref) (n : float) = r := !r +. n
let ( -.= ) (r : float ref) (n : float) = r := !r -. n
let ( *.= ) (r : float ref) (n : float) = r := !r *. n
let ( /.= ) (r : float ref) (n : float) = r := !r /. n

let ( |> ) x f = f x
let ( >> ) f g x = f(g x)
let ( $ ) f x = f x
let swap f x y = f y x
let flip = swap

type label = int
type features = (int,float) Hashtbl.t
type meta = {global : string; local : string}
type instance = {label : int; features : features; info : meta}
type data = instance list

exception Done

let empty_instance = {label=0; features=(Hashtbl.create 0); info={local="";global=""}}

let list_of_array = Array.to_list

let array_of_list = Array.of_list

let bool_of_int i = if i = 0 then false else true
let int_of_bool b = if b then 1 else 0

let boi = bool_of_int
let iob = int_of_bool
let sob = string_of_bool
let bos = bool_of_string
let soi = string_of_int
let sof = string_of_float
let ios = int_of_string
let fos = float_of_string
let iof = int_of_float
let foi = float_of_int
let loa = list_of_array
let aol = array_of_list
let soc = string_of_char
let aoe = Array.of_enum
let eoa = Array.enum
let loe = List.of_enum
let eol = List.enum
let ioc = int_of_char
let coi = char_of_int
let char_of_string s = 
   if String.length s !=1 then 
      raise (Invalid_argument "length of string != 1");
   s.[0]
;;

let rev_compare a b = -1 * compare a b

(** String.nsplit in Batteries is currently completely broken... *)
let string_nsplit str sep =
  if str = "" then []
  else
    (* str is non empty *)
    let seplen = String.length sep in
    let rec aux acc ofs =
      if ofs >= 0 then (
        match
          try Some (String.rfind_from str ofs sep)
          with String.Invalid_string -> None
        with
          | Some idx -> (* sep found *)
            let end_of_sep = idx + seplen - 1 in
              if end_of_sep = ofs (* sep at end of str *)
              then aux (""::acc) (idx - 1)
              else
                let token = String.sub str (end_of_sep + 1) (ofs - end_of_sep) in
                  aux (token::acc) (idx - 1)
          | None     -> (* sep NOT found *)
            (String.sub str 0 (ofs + 1))::acc
      )
      else
        (* Negative ofs: the last sep started at the beginning of str *)
        ""::acc
    in
      aux [] (String.length str - 1 )
;;

let tuple1_print ?(first="(") ?(last=")") p1 out (x1) =
   IO.nwrite out first;
   p1 out x1;
   IO.nwrite out last
;;
let tuple2_print ?(first="(") ?(last=")") ?(sep=", ") p1 p2 out (x1,x2) =
   IO.nwrite out first;
   p1 out x1;
   IO.nwrite out sep;
   p2 out x2;
   IO.nwrite out last
;;
let tuple3_print ?(first="(") ?(last=")") ?(sep=", ") p1 p2 p3 out (x1,x2,x3) =
   IO.nwrite out first;
   p1 out x1;
   IO.nwrite out sep;
   p2 out x2;
   IO.nwrite out sep;
   p2 out x3;
   IO.nwrite out last
;;
let tuple4_print ?(first="(") ?(last=")") ?(sep=", ") p1 p2 p3 p4 out (x1,x2,x3,x4) =
   IO.nwrite out first;
   p1 out x1;
   IO.nwrite out sep;
   p2 out x2;
   IO.nwrite out sep;
   p2 out x3;
   IO.nwrite out sep;
   p2 out x4;
   IO.nwrite out last
;;

(****************************************************************************************)

module Da = struct
include Dyn_array

let mem da e =
   try
      for i = 0 to length da - 1 do
         let e' = get da i in
         if e = e' then raise Done
      done;
      raise Not_found
   with Done -> true | Not_found -> false
;;

let memq da e =
   try
      for i = 0 to length da - 1 do
         let e' = get da i in
         if e == e' then raise Done
      done;
      raise Not_found
   with Done -> true | Not_found -> false
;;

   
(* remove element e (uses physical equality!!) *)
let remove da e =
   try
      for i = 0 to length da - 1 do
         let e' = get da i in
         if e == e' then 
            (delete da i; raise Done)
      done;
      raise Not_found
   with Done -> ()
;;
end

(****************************************************************************************)

module A = struct
include Array

let last a = 
   let idx = length a - 1 in
   unsafe_get a idx
;;
let iter2 f a1 a2 = 
   let len1, len2 = length a1, length a2 in
   if len1 != len2 then 
      invalid_arg (Printf.sprintf "iter2: arrays of different size: %d,%d" len1 len2);
   for i = 0 to len1 - 1 do
      f (unsafe_get a1 i) (unsafe_get a2 i) 
   done
;;
let iter2i f a1 a2 = 
   let len1, len2 = length a1, length a2 in
   if len1 != len2 then 
      invalid_arg "iter2: arrays of different size";
   for i = 0 to len1 - 1 do
      f i (unsafe_get a1 i) (unsafe_get a2 i) 
         done
;;

let switer a f = iter f a
let switeri a f = iteri f a
let switer2 a1 a2 f = iter2 f a1 a2
let switer2i a1 a2 f = iter2i f a1 a2

let map2 f a1 a2 = 
   let l1 = length a1 
   and l2 = length a2 in
   if l1 != l2 then
      invalid_arg (sprintf "map2: arrays of different size: %d and %d" l1 l2);
   if l1 = 0 then [||] else begin
      let r = create l1 (f (unsafe_get a1 0) (unsafe_get a2 0)) in
      for i = 1 to l1 - 1 do
         unsafe_set r i (f (unsafe_get a1 i) (unsafe_get a2 i))
      done;
      r
   end
;;

let map_matrix f m =
   let l = length m in
   if l = 0 then [||] else begin
      let r = create l [||] in
      for i = 0 to l - 1 do
         unsafe_set r i (map f (unsafe_get m i))
      done;
      r
   end
;;
let map_2D = map_matrix
let mapij_2D f m = 
   let l = length m in
   if l = 0 then [||] else begin
      let r = create l [||] in
      for i = 0 to l - 1 do
         unsafe_set r i (mapi (f i) (unsafe_get m i))
      done;
      r
   end
;;
let iter_2D f a = 
   for i = 0 to length a - 1 do 
      let a' = unsafe_get a i in
      for j = 0 to length a' - 1 do
         f(unsafe_get a' j)
      done;
   done
;;
let iterij_2D f a =
   for i = 0 to length a - 1 do
      let a' = unsafe_get a i in
      for j = 0 to length a' - 1 do
         f i j (unsafe_get a' j)
      done;
   done
;;
let iter_3D f a = 
   for i = 0 to length a - 1 do
      let a' = unsafe_get a i in
      for j = 0 to length a' - 1 do
         let a'' = unsafe_get a' j in
         for k = 0 to length a'' - 1 do
            f(unsafe_get a'' k)
         done;
      done;
   done
;;
let iterijk_3D f a = 
   for i = 0 to length a - 1 do
      let a' = unsafe_get a i in
      for j = 0 to length a' - 1 do
         let a'' = unsafe_get a' j in
         for k = 0 to length a'' - 1 do
            f i j k (unsafe_get a'' k)
         done;
      done;
   done
;;

let smap a f = map f a
let smapi a f = map f a
let smap2 a1 a2 f = map2 f a1 a2

(** in-place shuffle *)
let shuffled_in ?(seed=None) a =
   if length a = 0 then 
      raise (Invalid_argument "can't shuffle an empty array");
   (match seed with 
      | Some n -> Random.init n
      | _ -> ());
   let rec fy a = function  (* fisher-yates shuffle algorithm *)
      | 0 -> a
      | i -> 
         let swap_pos = Random.int i in
         let temp = a.(i) in
         a.(i) <- a.(swap_pos);
         a.(swap_pos) <- temp;
         fy a (pred i) 
   in
   fy a (length a - 1)
;;

(** non-destructive shuffle *)
let shuffled ?(seed=None) a =    
   (* XXX ignore warning *)
   let a' = copy a in
   ignore (shuffled_in a'); a' 
;;

(** imperative (destructive) shuffle*)
let shuffle a = ignore (shuffled_in a)  

(** [put f a] replaces the [i]th element of array [a] with [f i]*)
let put f a =
   for i = 0 to length a - 1 do
      unsafe_set a i (f i)
   done
;;

(** [update f a] replaces each element [e] in array [a] with [f e]*)
let update f a =
   for i = 0 to length a - 1 do
      unsafe_set a i (f (unsafe_get a i)) 
   done
;;

(** [update f a] replaces each ith element [e] in array [a] with [f i e]*)
let updatei f a = 
   for i = 0 to length a - 1 do
      unsafe_set a i (f i (unsafe_get a i))
   done
;;

(** [unique a] returns a new array [a'] with all the unique elements of [a] appearing
    exactly once, in the intial order of appearance in [a]. *)
let unique a = 
  let h = Hashtbl.create (length a * 2) 
  and idx = ref 0 in
  iter 
   (fun e -> 
      if not (Hashtbl.mem h e) then
         (Hashtbl.add h e !idx; 
         incr idx))   
    a;
  let a' = make (Hashtbl.length h) a.(0) in
  Hashtbl.iter (fun e i -> a'.(i) <- e) h;
  a'
;;

(** [to_ht a] returns a hashtable [h] where [Ht.find h elt = i], for some [i] s.t. [a.(i) = elt] *)
let to_ht a = 
   let res = Hashtbl.create (2 * length a) in
   iteri 
      (fun i e -> Hashtbl.add res e i) a;
   res
;; 

(** [except i a] returns a copy of array [a] excluding element index [i]*)
let except i a = 
   init (length a - 1) (fun j -> if j < i then a.(j) else a.(j+1))
;;

let find_item e a = 
   let n = length a in
   let rec loop i =
      if i = n then raise Not_found
      else if unsafe_get a i = e then i
   else loop (succ i)
   in
   loop 0
;;
(** 
   [find_alli p a] returns the index array of elements from 
   array [a] that satisfy predicate [p] 
*)
let find_alli p a =
   let n = length a in
   let bs = BitSet.create n in
   for i = 0 to n-1 do
      if p a.(i) then BitSet.set bs i
   done;
   let n' = BitSet.count bs in
   let j = ref 0 in
   let a' = init n'
      (fun _ ->
         while not (BitSet.is_set bs !j) do incr j done;
         let r = !j in
         incr j; 
         r) in
   a'
;; 

let existsi p a = 
   let n = length a in
   let rec loop i =
      if i = n then false
      else if p i a.(i) then true
      else loop (succ i)
   in
   loop 0
;;

let slice ?(first=0) ?(last=Sys.max_array_length) a =
   let clip (_min:int) (_max:int) x = Standard.max _min (Standard.min _max x) in
   let i = clip 0 (length a)
      (if (first<0) then (length a) + first else first)
   and j = clip 0 (length a)
      (if (last<0) then (length a) + last else last)
   in
   if i>=j || i=length a then
      [||]
        else
            sub a i (j-i)
;;

(* [split a i] returns a pair of arrays made by splitting [a]:  the first [i] elements go in the first array*)
let split a i = 
   sub a 0 (i), sub a i (length a - i)
;;

(** [range fst lst] returns an array of ints from [fst] inclusive to [lst] exclusive *)
let range fst lst = Array.init (lst - fst) (fun i -> i + fst)

let findi_next first p a =
   let n = length a in
   let rec loop i =
      if i = n then raise Not_found
      else if p a.(i) then i
      else loop (succ i)
   in
   loop first
;;
let find_next first p a = a.(findi_next first p a)

let rand_iter ?(seed=None) f a =
   let len = length a in 
   let order = shuffled_in ~seed:seed (range 0 len) in
   for i = 0 to len - 1 do f(unsafe_get a (unsafe_get order i)) done
;;
let rand_iteri ?(seed=None) f a =
   let len = length a in
   let order = shuffled_in ~seed:seed (range 0 len) in
   for i = 0 to len - 1 do 
      let j = unsafe_get order i in
      f j (unsafe_get a j)
   done
;;

(* XXX if the string representation of an element uses 'btw', it will be misinterpreted *)
let of_str ~btw:btw ~l:l ~r:r f s = 
   if not (String.starts_with s l && String.ends_with s r) then 
      raise (Invalid_argument (s ^ " not encased by " ^ l ^ r));
   let s' = String.slice ~first:(String.length l) ~last:( - String.length l) s in
   let a = aol (string_nsplit s' btw) in
   map f a
;;
let of_str1 f s = of_str ~btw:" " ~l:"" ~r:"" f s
let of_str2 f s = of_str ~btw:", " ~l:"[" ~r:"]" f s

let to_str ~btw:btw ~l:l ~r:r f a = 
   let lst = to_list (map f a) in
   l ^ String.join btw lst ^ r
;;    
let to_str1 f a = to_str ~btw:" " ~l:"" ~r:"" f a
let to_str2 f a = to_str ~btw:"; " ~l:"[|" ~r:"|]" f a
let print f a = print_endline (to_str2 f a)

let all_same a = 
   let rec samei a2 i = 
      if i = length a2 then true else a2.(0) = a2.(i) && samei a2 (i + 1) 
   in
   samei a 1
;;

let normalize a = 
   let z = ref 0. in
   let n = length a - 1 in
   for i = 0 to n do
      z := !z +. unsafe_get a i
   done;
   if !z = 0. then
      invalid_arg "normalize: normalization constant is 0!";
   for i = 0 to n do 
      unsafe_set a i (unsafe_get a i /. !z)
   done
;;

let old_count a e =
   let res = ref 0 in
   for i = 0 to length a - 1 do
      if unsafe_get a i = e then incr res
   done;
   !res
;;
let count pred a = 
   let res = ref 0 in
   for i = 0 to length a - 1 do
      let e = unsafe_get a i in
      if pred e then incr res
   done;
   !res
;;
let incr a i = a.(i) <- a.(i) + 1
let decr a i = a.(i) <- a.(i) - 1

let incr' a i = a.(i) <- a.(i) +. 1.0
let decr' a i = a.(i) <- a.(i) -. 1.0

let fill_all a v = 
   for i = 0 to length a - 1 do unsafe_set a i v done
;;

let init_matrix sx sy f =
   let res = create sx [||] in
   for x = 0 to pred sx do
      let f' = f x in
      unsafe_set res x (init sy f')
   done;
   res
;;
let init_2D = init_matrix

let init_symm_matrix s f =
   if s = 0 then [||]
   else begin
      let res = make_matrix s s (f 0 0) in
      for x = 0 to s - 1 do
         unsafe_set (unsafe_get res x) x (f x x);
         for y = 0 to x - 1 do
            let elt = f x y in
            unsafe_set (unsafe_get res x) y elt;
            unsafe_set (unsafe_get res y) x elt
         done
      done;
      res
   end
;;

let make_2D = make_matrix
let make_3D sx sy sz init = 
   let res = create sx [||] in
   for x = 0 to pred sx do
      unsafe_set res x (create_matrix sy sz init)
   done;
   res
;;

let init_3D sx sy sz f = 
   let res = create sx [||] in
   for x = 0 to pred sx do
      let f' = f x in
      unsafe_set res x (init_matrix sy sz f')
   done;
   res
;;

let fill_all_matrix m v = 
   for i = 0 to length m - 1 do
      let a = unsafe_get m i in
      for j = 0 to length a - 1 do
         unsafe_set a j v
      done;
   done
;;
let fill_all_2D = fill_all_matrix
let fill_3D a v = 
   for i = 0 to length a - 1 do
      let a' = unsafe_get a i in
      for j = 0 to length a' - 1 do
         let a'' = unsafe_get a' j in
         for k = 0 to length a'' - 1 do
            unsafe_set a'' k v
         done;
      done;
   done
;;

let copy_matrix m = 
   let l = length m in
   if l = 0 then [||] else begin
      let res = create l (copy (unsafe_get m 0)) in
      for i = 1 to pred l do
         unsafe_set res i (copy (unsafe_get m i))
      done;
      res
   end
;;
let copy_2D = copy_matrix
let sum a = 
   let z = ref 0. in
   for i = 0 to length a - 1 do
      z := !z +. (unsafe_get a i)
   done;
   !z
;;

let sum_int a = 
   let z = ref 0 in
   for i = 0 to length a - 1 do
      z := !z + (unsafe_get a i)
   done;
   !z
;;

let filteri f a = 
   let n = length a in
   let bs = BitSet.create n in 
   for i = 0 to n - 1 do
      if (f i (unsafe_get a i)) then BitSet.set bs i
   done;
   let n' = BitSet.count bs in
   let j = ref 0 in
   let a' = init n' 
      (fun _ ->
         while not (BitSet.is_set bs !j) do Pervasives.incr j done;
         let r = unsafe_get a !j in
         Pervasives.incr j; r) 
   in a'
;;
let flatten a = 
   let len = ref 0 in
   for i = 0 to length a - 1 do
      len := !len + length (unsafe_get a i)
   done;
   let i,j = ref 0, ref 0 in
   init !len 
   (fun _ ->
      while !j = length (unsafe_get a !i) do
         i := !i + 1;
         j := 0
      done;
      let v = unsafe_get (unsafe_get a !i) !j in
      j := !j + 1;
      v)
;;

let concat = flatten

let map_filter f a = 
   let a' = map f a in   
   let n = length a' in
   let bs = BitSet.create n in
   for i = 0 to n - 1 do
      if unsafe_get a' i != None then BitSet.set bs i
   done;
   let n' = BitSet.count bs in
   let j = ref 0 in
   let a'' = init n'
      (fun _ ->
         while not (BitSet.is_set bs !j) do Pervasives.incr j done;
         let r = Opt.get (unsafe_get a' !j) in
         Pervasives.incr j; r)
   in a''
;;
let filter_map = map_filter;;
let map_filteri f a = 
   let a' = mapi f a in   
   let n = length a' in
   let bs = BitSet.create n in
   for i = 0 to n - 1 do
      if unsafe_get a' i != None then BitSet.set bs i
   done;
   let n' = BitSet.count bs in
   let j = ref 0 in
   let a'' = init n'
      (fun _ ->
         while not (BitSet.is_set bs !j) do Pervasives.incr j done;
         let r = Opt.get (unsafe_get a' !j) in
         Pervasives.incr j; r)
   in a''
;; 

let iter_all f a = iter f (flatten a)
let iteri_all f a = iteri f (flatten a)
   
let filter_all f a = filter f (flatten a)
let filteri_all f a = filteri f (flatten a)
let map_all f a = map f (flatten a)
let mapi_all f a = mapi f (flatten a)
let map_filter_all f a = map_filter f (flatten a)
let map_filteri_all f a = map_filteri f (flatten a)
let filter_all_ij f a = 
   let res = make (length a) [||] in
   for i = 0 to length a -1 do
      unsafe_set res i (filteri (f i) (unsafe_get a i))
   done; flatten res
;;
let map_all_ij f a = 
   let res = make (length a) [||] in
   for i = 0 to length a - 1 do
      unsafe_set res i (mapi (f i) (unsafe_get a i))
   done; flatten res
;;
let iter_all_ij f aa = 
   for i = 0 to length aa - 1 do
      let a = unsafe_get aa i in 
      for j = 0 to length a - 1 do
         f i j (unsafe_get a j)
      done
   done
;;
let map_filter_all_ij f a = 
   let res = make (length a) [||] in
   for i = 0 to length a - 1 do
      unsafe_set res i (map_filteri (f i) (unsafe_get a i))
   done; flatten res
;;

let get_combos (a : 'a array array) = 
   let lengths = map length a in
   assert (for_all ((<) 0) lengths);
   if length a = 0 then [||] else begin
   let counter = make (length a) 0 in
   let idx = ref 0 in
   let rec advance () = 
      let res = ref true in
      if (unsafe_get counter !idx) >= (unsafe_get lengths !idx) - 1 then begin
            if !idx = length counter - 1 then
               res := false
            else begin
               unsafe_set counter !idx 0;
               Pervasives.incr idx;
               res := advance () 
            end
         end
      else begin
         unsafe_set counter !idx ((unsafe_get counter !idx) + 1);
         idx := 0
      end;
      !res
   in
   let res = make (fold_left ( * ) 1 lengths) [||] in
   let idx = ref 0 in
   let add () = 
      unsafe_set res !idx (mapi (fun i j -> unsafe_get (unsafe_get a i) j) counter)
   in
   add ();
   Pervasives.incr idx;
   while (advance ()) do
      add ();
      Pervasives.incr idx
   done;
   assert (!idx = length res);
   res end
;;
   
let max f a =
   let len = length a in
   if len = 0 then 
      failwith "A.max: Array of length zero!";
   let res   = ref (unsafe_get a 0) in
   let f_res = ref (f !res) in   
   for i = 1 to len - 1 do
      let elt = unsafe_get a i in
      let f_elt = f elt in
      if f_elt > !f_res then begin
         f_res := f_elt;
         res   := elt
      end
   done;
   !res
;;
   
let combine a1 a2 =
   let len1 = length a1 in
   let len2 = length a2 in
   if not (len1 = len2) then
      raise (Invalid_argument "combine: arrays of different length");
   init len1 (fun i -> (a1.(i),a2.(i)))
;;

end

(****************************************************************************************)

module L = struct
include List

let min_itemi l =
   let rec f mni mn i = function
      | e :: [] -> 
         if e < mn then (i ,e) 
            else (mni, mn)
      | e :: tl ->
         if e < mn then f i e (i+1) tl 
            else f mni mn (i+1) tl
      | [] -> raise (Failure "min_itemi: list of length 0")
   in match l with
   | e :: [] -> (0, e)
   | e :: tl -> f 0 e 1 tl
   | [] -> raise (Failure "min_itemi: list of length 0")
;;

let max_itemi l =
   let rec f mni mn i = function
      | e :: [] -> 
         if e > mn then (i ,e) 
            else (mni, mn)
      | e :: tl ->
         if e > mn then f i e (i+1) tl 
            else f mni mn (i+1) tl
      | [] -> raise (Failure "min_itemi: list of length 0")
   in match l with
   | e :: [] -> (0, e)
   | e :: tl -> f 0 e 1 tl
   | [] -> raise (Failure "min_itemi: list of length 0")
;;


let iter2i f l1 l2 = 
   let rec loop n l1 l2 =
      match l1, l2 with
      | [], [] -> ()
      | h1 :: t1, h2 :: t2 -> 
         f n h1 h2; loop (n+1) t1 t2
      | _ -> raise (Different_list_size "iter2i")
   in
   loop 0 l1 l2
;;

let switer l f = iter f l 
let switeri l f = iteri f l
let switer2 l1 l2 f = iter2 f l1 l2
let switer2i l1 l2 f = iter2i f l1 l2

let smap l f = map f l
let smapi l f = map f l
let smap2 l1 l2 f = map2 f l1 l2


let all_same l =
   match l with
   | [] -> true
   | [e] -> true
   | hd :: tl -> fold_left (fun b e -> b && (e = hd)) true tl
;;

let is_empty = function
   | [] -> true
   | _ -> false
;;
(* XXX if the string representation of an element uses 'btw', it will be misinterpreted *)
let of_str ~btw:btw ~l:l ~r:r f s = 
   if not (String.starts_with s l && String.ends_with s r) then 
      raise (Invalid_argument (s ^ " not encased by " ^ l ^ r));
   let s' = String.slice ~first:(String.length l) ~last:( - String.length l) s in
   if s' = "" then []
   else
      let a = string_nsplit s' btw in
      map f a
;;
let of_str1 f s = of_str ~btw:" " ~l:"" ~r:"" f s
let of_str2 f s = of_str ~btw:", " ~l:"[" ~r:"]" f s

let to_str ~btw:btw ~l:l ~r:r f a = 
   let lst = map f a in
   l ^ String.join btw lst ^ r
;;    
let to_str1 f a = to_str ~btw:" " ~l:"" ~r:"" f a
let to_str2 f a = to_str ~btw:", " ~l:"[" ~r:"]" f a

let count l e =
   fold_left (fun acc e' -> if e'=e then succ acc else acc) 0 l
;;

let unique l = 
   let h = Hashtbl.create (2 * length l) in
   let rec f l acc = 
      match l with
      | []     ->  acc
      | x :: t -> 
         if Hashtbl.find h x then f t acc
         else (Hashtbl.add h x true; f t (x :: acc))
   in
   rev (f l [])
;;

let sum l =
   let rec f l' acc =
      match l' with
         | h::t -> f t (acc +. h)
         | [] -> acc
   in
   f l 0.
;;

let range a b =
   let rec f a b acc =
      if b < a then acc
      else f a (pred b) (b :: acc)
   in
   f a (b - 1) []
;;

(** transposes a list of lists as if it were a matrix *)
let rec transpose1 (ll : 'a list list) = 
   match ll with
   | [] :: t -> []
   | h  :: t -> map hd ll :: transpose1 (map tl ll)
   | _       -> invalid_arg "can't transpose an empty list"
;;
(* tail recursive version! *)
let transpose2 (ll : 'a list list) = 
   let rec aux ll acc = 
      match ll with
      | [] :: t -> acc
      | h  :: t -> aux (map tl ll) (map hd ll :: acc)   
      | _       -> invalid_arg "can't transpose an empty list"
   in
   rev (aux ll [])
;;
let transpose = transpose2
let map_filter = filter_map

(****************************************************************************************)
end

(****************************************************************************************)

module Ht = struct
include Hashtbl

let add_int (tbl : ('a,int) t) (key : 'a) (n : int) = 
  if mem tbl key then 
    replace tbl key ((find tbl key) + n)
  else
    add tbl key n
;;
let add_float (tbl : ('a,float) t) (key : 'a) (n : float) = 
  if mem tbl key then 
    replace tbl key ((find tbl key) +. n)
  else
    add tbl key n
;;
let incr tbl key = add_int tbl key 1
let decr tbl key = add_int tbl key (-1)
let incr' tbl key = add_float tbl key 1.0
let decr' tbl key = add_float tbl key (-1.0)
let get (tbl : ('a,'b) t) (key : 'a) (default : 'b) =
  if mem tbl key then 
    find tbl key 
  else
    default
;;
let count  tbl key = if mem tbl key then find tbl key else 0
let count' tbl key = if mem tbl key then find tbl key else 0.
let sum  tbl = fold (fun _ n acc -> n + acc) tbl 0;;
let sum' tbl = fold (fun _ n acc -> n +. acc) tbl 0.;;

let max_item tbl =
   let e = enum tbl in
   Enum.reduce (fun (k,n) (k',n') -> if n' > n then (k',n') else (k,n)) e
;;
let to_str f1 f2 btw l r map ht =
   let a = aoe (keys ht) in
   A.fast_sort compare a;
   l ^ String.join btw (loa (A.map (fun k -> f1 k ^ map ^ f2 (find ht k)) a)) ^ l
;;
let to_str1 f1 f2 ht = to_str f1 f2 "\n" "" "" "\t->\t" ht

(** [aggregrate ht1 ht2] aggregates counts from two integer hashtables *)
let aggregrate ht1 ht2 =
   let res = create 500 in
   iter (fun k n -> add_int res k n) ht1;
   iter (fun k n -> add_int res k n) ht2;
   res
;;   
(** [aggregrate' ht1 ht2] aggregates counts from two float hashtables *)
let aggregrate' ht1 ht2 =
   let res = create 500 in
   iter (fun k n -> add_float res k n) ht1;
   iter (fun k n -> add_float res k n) ht2;
   res
;;   


(** [of_file key_fun val_fun fn] returns a hashtable from a file [fn].  Each line of 
[fn] should contain a key,value pair separated by tab *)
let of_file key_fun val_fun fn =
   let lines = Std.input_list (Pervasives.open_in fn) in
   let res = create 500 in
   L.iter 
      (fun l ->
         try 
            let key_s, val_s = String.split l "\t" in
            add res (key_fun key_s) (val_fun val_s)
         with ExtString.Invalid_string -> 
            raise (Invalid_argument ("each line in file must have a SINGLE TAB character separating key from value: \"" ^ l ^ "\"")))
      lines;
   res
;;

let reverse ht = 
   let res = create (length ht * 2) in
   iter (fun k v -> add res v k) ht;
   res
;;

(** [of_arrays k v] returns a hashtable where [k.(i)] -> [v.(i)] for each [i].*)
let of_arrays k v = 
   let len = A.length k in
   if len <> A.length v then
      raise (Invalid_argument "Ht.of_arrays: arrays of unequal length");
   let res = create (3 * len) in
   for i = 0 to len - 1 do
      add res (Array.unsafe_get k i) (Array.unsafe_get v i)
   done;
   res
;;

(** [of_array a] returns a hashtable [h] where [Ht.find h elt = i], for some [i] s.t. [a.(i) = elt] *)
let of_array a = A.to_ht a

let to_array ht = aoe (enum ht)
let to_sorted_array ht = 
   let res = to_array ht in
   A.fast_sort (fun (k,v) (k',v') -> compare v v') res; 
   res
;;
let to_rev_sorted_array ht = 
   let res = to_array ht in
   A.fast_sort (fun (k,v) (k',v') -> rev_compare v v') res;
   res
;; 

let init a f = 
   let res = create (2 * A.length a) in
   A.iter (fun e -> replace res e (f e)) a;
   res
;;

(** [add_if_empty h k v] maps if key [k] to value [v] in hashtable [h] only if [h] doesn't already contain a value for [k] *)
let add_if_empty h k v =
   if not (mem h k) then
      add h k v
;;
   

end

(****************************************************************************************)

module S = struct

include String

let count s c =
   fold_left (fun acc c' -> if c'=c then succ acc else acc) 0 s
;;

let lpad n s =
   let len = length s in
   if len >= n then s
   else
      make (n - len) ' ' ^ s
;;

let rpad n s =
   let len = length s in
   if len >= n then s
   else
     s ^  make (n - len) ' '
;;

let lstrip ?(chars=" \t\r\n") s =
   let p = ref 0 in
   let l = length s in
   while !p < l && contains chars (unsafe_get s !p) do
      incr p;
   done;
   sub s !p (l - !p)
;;
let rstrip ?(chars=" \t\r\n") s =
   let l = ref (length s - 1) in
   while !l >= 0 && contains chars (unsafe_get s !l) do
      decr l;
   done;
   sub s 0 (!l + 1)
;;
let strip ?(chars=" \t\r\n") s = lstrip ~chars (rstrip ~chars s)
let remove c s =
   replace_chars (fun c' -> if c'=c then "" else soc c') s
;;
let replace_all ~str ~sub ~by =
   let str_len = length str in
   let sub_len = length sub in
   let res = Buffer.create (2 * str_len) in
   let i = ref 0 in
   while !i < str_len do
      if slice ~first:!i ~last:(!i + sub_len) str = sub then begin
         Buffer.add_string res by;
         i += sub_len
      end
      else begin
         Buffer.add_char res str.[!i];
         incr i
      end
   done;
   Buffer.contents res
;;
let nsplit = string_nsplit
let nsplita s sep = aol (nsplit s sep)
let concata s sa = concat s (loa sa)
let joina s sa = join s (loa sa)

end

(****************************************************************************************)

module E = struct

include Enum

let switer e f = iter f e
let switeri e f = iteri f e
let switer2 e1 e2 f = iter2 f e1 e2
let switer2i e1 e2 f = iter2i f e1 e2

end

(****************************************************************************************)

module Pair = struct

let map f (a,b) = (f a, f b)
let swap (a,b) = (b,a)
let iter f (a,b) = f a; f b

end

(****************************************************************************************)

module G = struct

include Global

let notdef g = not (isdef g)

let create ?(s="unnamed global variable") v = 
   let res = empty s in
   set res v;
   res
;;

end

(****************************************************************************************)

module Hs = struct

type 'a t = ('a, bool) Ht.t

let create = Ht.create
let add hs v = if not (Ht.mem hs v) then Ht.add hs v true
let remove = Ht.remove
let mem = Ht.mem
let length = Ht.length
let iter f hs = Ht.iter (fun k _ -> f k) hs

let keys hs = Ht.keys hs
let to_array hs = A.of_enum (Ht.keys hs)
let to_list hs = L.of_enum (Ht.keys hs)
let of_array a = 
   let res = create (A.length a * 2) in
   A.iter (fun v -> add res v) a;
   res
;;
let of_list l = 
   let res = create (L.length l * 2) in
   L.iter (fun v -> add res v) l;
   res
;;


end

(****************************************************************************************)

let eq x y = x = y
let gt x y = x > y
let gte x y = x >= y
let lt x y = x < y
let lte x y = x <= y

let pause () = 
   print_string "\n(PAUSED)";
   flush stdout;
   let _ = input_line stdin in
   print_string "\n\n";
   flush stdout
;;

let get_time_string () =
   let tm = Unix.localtime (Unix.time ()) in
   let year = soi (1900 + tm.Unix.tm_year) in
   let month = soi tm.Unix.tm_mon in
   let tz = if tm.Unix.tm_isdst then "EDT" else "EST" in
   let day = soi tm.Unix.tm_mday in
   let time = (soi tm.Unix.tm_hour) ^ ":" ^ (soi tm.Unix.tm_min) ^ ":" ^ (soi tm.Unix.tm_sec) in
   month ^ "/" ^ day ^ "/" ^ year ^ " " ^ time ^ " " ^ tz
;;

let my_dump a = 
   let b = Obj.repr a in
   if Obj.tag b = Obj.string_tag then (Obj.magic a : string)
   else Std.dump a

let time f =
   let t = Unix.gettimeofday () in
   f ();
   Unix.gettimeofday () -. t
;;

let list_of_file file_name =
  let in_ch = Pervasives.open_in file_name in
  Std.input_list in_ch
;;
let array_of_input input =
  ExtArray.Array.of_enum (Std.input_lines input)
;;  
let array_of_file file_name = 
  array_of_input (Pervasives.open_in file_name)
;;
let enum_of_file file_name =
  Std.input_lines (Pervasives.open_in file_name)
;;
let output_enum out_ch (enum : string E.t) =
  E.iter (fun s -> output_string out_ch s; output_string out_ch "\n") enum;
  flush out_ch
;;
let file_of_enum file_name (enum : string E.t) =
  let out_ch = open_out file_name in
  output_enum out_ch enum;
  close_out out_ch
;;
let output_list out_ch (list : string list) = 
  output_enum out_ch (L.enum list)
;;  
let file_of_list file_name (list : string list) = 
  file_of_enum file_name (L.enum list)
;;
let output_array out_ch (array : string array) =
  output_enum out_ch (A.enum array)
;;
let file_of_array file_name (array : string array) = 
  file_of_enum file_name (A.enum array)
;;
let dot_prd_ff (features1 : features) (features2 : features) = 
  let mult feat feat_val sum = 
    if (Ht.mem features2 feat) then
      feat_val *. (Ht.find features2 feat) +. sum
    else sum in
  Ht.fold mult features1 0.
;;
let dot_prd_fw (features : features) (weights : float array) = 
  let mult feat feat_val sum = 
    try
      weights.(feat) *. feat_val +. sum
    with Invalid_argument s -> sum in
  Ht.fold mult features 0.
;;
let split ?(max=max_int) (s : string) = 
   Str.bounded_split (Str.regexp "[ \t\n\r]+") s max
;;
let splita ?(max=max_int) (s : string) = 
   aol (split ~max s)
;;

(** [lines_of_str s] returns (XXX non-empty), non-commented lines of string [s], and strips off comments *)
(* let lines_of_str ?(comment_str="#") str =
   S.strip ~chars:"\n" str                                        |>
   (fun s -> S.nsplita s "\n")                                    |>
   A.map (fun s -> try fst (S.split s comment_str) with _ -> s)   |>
   A.filter ((<>) "") 
;; *)
let lines_of_str ?(comment_str="#") str = 
   S.strip ~chars:"\n" str                                        |>
   (fun s -> S.nsplita s "\n")                                    |>
   A.filter (fun s-> not (S.starts_with s comment_str))           |>
   A.map (fun s -> try fst (S.split s comment_str) with _ -> s)
;;

(** [lines_of_file f] returns non-empty, white-space stripped, non-commented lines of file [f] *)
let lines_of_file ?(comment_str="#") fd =
   (* let in_ch = Pervasives.open_in f in 
   let res = lines_of_str (input_all in_ch) in
   close_in in_ch;
   res *)
   let lines = Std.input_list (Pervasives.open_in fd) in
   let f ln = 
      let ln = try 
         fst (S.split ln comment_str) 
         with _ -> ln in
      let ln = S.strip ln in
      if ln = "" then None else Some ln
   in
   aol (L.map_filter f lines)   
;;

let min_itemi a =
  let l = A.length a in
  if l = 0 then raise (Failure "array of length 0")
  else
     let min_i, min_e = ref 0, ref a.(0) in
    for i = 1 to l - 1 do
      let e = a.(i) in
      if e < !min_e then 
  (min_i := i; min_e := e)
    done;
    !min_i, !min_e
;;
let min_item a = snd (min_itemi a)
let max_itemi a =
  let l = A.length a in
  if l = 0 then raise (Failure "array of length 0")
  else
    let max_i, max_e = ref 0, ref a.(0) in
    for i = 1 to l - 1 do
      let e = a.(i) in
      if e > !max_e then 
  (max_i := i; max_e := e)
    done;
    !max_i, !max_e
;;
let max_item a = snd (max_itemi a)

let max_in_ht tbl =
   let mx_k, mx_v = match E.peek (Ht.enum tbl) with
      | None -> raise (Failure "empty hashtable")
      | Some (k,v) -> ref k, ref v in
   Ht.iter 
      (fun k v -> if v >= !mx_v then (mx_k := k; mx_v := v))
      tbl;
      (!mx_k,!mx_v)
;;

let min_in_ht tbl =
   let mn_k, mn_v = match E.peek (Ht.enum tbl) with
      | None -> raise (Failure "empty hashtable")
      | Some (k,v) -> ref k, ref v in
   Ht.iter 
      (fun k v -> if v <= !mn_v then (mn_k := k; mn_v := v))
      tbl;
      (!mn_k,!mn_v)
;;
            
let min_max_items a = 
  let l = A.length a in
  if l = 0 then failwith "array of length 0"
  else
    let mx, mn = ref a.(0), ref a.(0) in
    for i = 1 to l - 1 do
      let e = a.(i) in
      if e > !mx then mx := e;
      if e < !mn then mn := e
    done;
    !mn, !mx
;;
let pi = 4.0 *. atan 1.0

let factorial = open Batteries.Num in
(fun n -> 
   if n < Int 0 then failwith "factorial undefined on inputs < 0";
   let rec f n acc = 
      match n with
      | Int 0 -> acc
      | Int 1 -> acc
      | n -> f (n - (Int 1)) (acc * n)
   in
   f n (Int 1))
;;

let log_fact n = 
   let res = ref 0.0 in
   for k = 1 to n do
      res := !res +. log (foi k)
   done;
   !res
;;
let log_fact_approx n_ =
   let n = foi n_ in
   n *. log n -. 
   n +.
   log (n *. (1. +. 4. *. n *. (1. +. 2. *. n))) /. 6. +. 
   log pi /. 2.
;;

let fact n = 
   if n < 0 then failwith "factorial undefined on inputs < 0";
   let rec f n acc = 
      match n with
      | 0 -> acc
      | 1 -> acc
      | n -> f (n - 1) (acc * n)
   in
   f n 1
;;

let choose n k = (fact n) / ((fact k) * (fact (n-k)))

let log_choose n k = log_fact n -. (log_fact k +. log_fact (n-k))

let sum a1 a2 = 
  let l1, l2 = A.length a1, A.length a2 in
  if l1 <> l2 then failwith "arrays of unequal length"
  else
    A.init l1 (fun i -> a1.(i) +. a2.(i))
;;

let round x = int_of_float (floor (x +. 0.5))

let parse_features (fs_list : string list) =
   let tbl = Ht.create 15 in
   let add (fs : string) =
      match Str.split (Str.regexp ":") fs with
         | [f; f_val] ->   
            begin
               try Ht.replace tbl (int_of_string f) (float_of_string f_val)
               with _ -> raise (Failure ("couldn't parse(1): " ^ fs))
            end
         | _ -> failwith "Wrong feature format!!" in
   L.iter add fs_list; 
   tbl
;;

(* XXX in multi-mode, assumes each label is only one character *)
let parse_data_line ?(global="") ?(m=1) ?(single=None) (s : string) : instance list =
   assert (single = None || m = 1);
   let s,info = 
      match Str.bounded_split (Str.regexp "#") s 2 with
         | [fs; cs] -> S.strip fs, {global = global; local = S.strip cs}
         | [fs] -> S.strip fs, {global = global; local = ""} 
         | _ -> raise (Failure "encountered blank line!") in
   if m = 1 then  
      match split s with
         | label :: fs_list -> 
            let label = match single with
               | None   -> label
               | Some n -> soc (label.[n-1]) in 
            (try [{ label = int_of_string label; 
               features = parse_features fs_list;
               info = info }] 
            with _ -> raise (Failure ("couldn't parse(2): " ^ s)))
         | _ -> raise (Failure ("wrong input format!! " ^ s))
   else 
      match split s with
         | labels :: fs_list ->
            let features = parse_features fs_list in
            let rec get_instances s' = 
               let s, neg = 
                  if S.starts_with s' "-" then 
                     (String.sub s' 1 (String.length s' - 1), -1)
                  else
                     (s', 1) in
               if String.length s = 1 then 
                  (try [{ label = neg * int_of_string s; 
                     features = features;
                     info = info }]
                  with _ -> raise (Failure ("couldn't parse(3): " ^ s)))
               else
               (try
                  { label = neg * int_of_string (String.sub s 0 1); 
                  features = features;
                  info = info } 
                  :: get_instances (Str.string_after s 1) 
               with _ -> raise (Failure ("couldn't parse(4): " ^ s))) in
            let res = get_instances labels in 
            if L.length res = m then
               res
            else
               raise (Failure ("didn't find " ^ string_of_int m ^ " single-character labels!!" ^ s))
         | _ -> raise (Failure ("wrong input format(2)!!" ^ s))
;;

let parse_data_file ?(verbose=1) ?(multi=false) ?(single=None) data_file : data * int = 
   let global = ref "" in
   let rec get_next_line df = 
      let res = S.strip (input_line df) in
      if res.[0] = '.' then 
         begin
            (match Str.bounded_split (Str.regexp ".") res 1 with
            | [g] -> global := S.strip g
            | [] -> global := ""
            | _ -> failwith "shouldn't happen 33");
            get_next_line df
         end
      else res 
   in
   let fst_ln = get_next_line data_file in
   let m = if multi then    
      String.length (Str.global_replace (Str.regexp "-") "" (L.hd (split fst_ln)))   
   else 1 in
   let instances = Dray.make 1000 in
   L.iter (Dray.add instances) (parse_data_line ~global:!global ~m ~single fst_ln);
   let count = ref 0 in
   (try while true do
      (if !count mod 50 = 0 && verbose > 1 then
         (print_int !count; print_string "...");      
      incr count;
      let ln = get_next_line data_file in
      L.iter (Dray.add instances) (parse_data_line ~global:!global ~m ~single ln)) 
   done
   with End_of_file -> ());
  flush stdout;
  (Dray.to_list instances), m
    (* let rec go (accum : instance list) =  *)
(*     try *)
(*       let ln = get_next_line data_file in *)
(*       go ((parse_data_line ~global:!global ~m ln) @ accum) *)
(*     with End_of_file -> accum in *)
(*   (L.rev (go (parse_data_line ~global:!global ~m fst_ln)), m) *)
;;
let get_labels (data : data) : label array =
   let l = Ht.create 15 in
   let f inst = Ht.replace l inst.label 1 in
   L.iter f data;
   A.of_enum (Ht.keys l)
;;
let get_highest_feature (data : data) : int = 
   let highest_in_instance (instance : instance) =
      let f feat feat_val prev_high = max feat prev_high in
      Ht.fold f instance.features 0 
   in
   L.fold_left max 0 (L.map highest_in_instance data)
;;
let sign a = if a < 0 then 1 else -1
let get_mean_dev l =
   let sum, num = L.fold_left ( +. ) 0. l, float_of_int (L.length l) in
   let mean = sum /. num in
   let dev = sqrt ((L.fold_left (fun sum v -> sum +. (v -. mean)**2.) 0. l) /. num) in
   (mean, dev)
;;
let id x = x
let deep_copy da = 
   let l = Dray.length da in
   Dray.init l (fun i -> ref !(Dray.get da i)) 
;;

(** 
    Takes a multi hashtable h and returns a new Hashtable with exactly one entry
    for each key in h, mapped to an array of all bindings of that key in h. 
*)
let mht_to_ht (h : ('a, 'b) Hashtbl.t) : ('a, 'b array) Hashtbl.t =
   let h' = Ht.create (Ht.length h) in
   Enum.iter 
      (fun k -> Ht.replace h' k (A.of_list (Ht.find_all h k)))
      (Ht.keys h);
   h'
;;

let hs_of_list = Hs.of_list
let hs_of_array = Hs.of_array 
let array_to_hs = hs_of_array
let output_line ch s = output_string ch (s ^ "\n")

let shuffle_list ?(seed=None) (l : 'a list) =
   loa (A.shuffled_in ~seed:seed (aol l))
;;
let list_except i l = 
   let rec f pos l acc = 
      match l with
         | [] -> (L.rev acc)
         | a :: tl -> 
            if pos < i then f (succ pos) tl (a :: acc)
            else (L.rev acc) @ tl 
   in
   f 0 l []
;;
(* memoize a NON-RECURSIVE funtion *)
let memoize f =
   let cache = Hashtbl.create 100 in
   fun n -> 
      try Hashtbl.find cache n
      with Not_found -> 
         let res = f n in 
         Hashtbl.add cache n res; res
;;          
(* memoize a RECURSIVE funtion (a little tricky -- see type of [f])*)
let memoize_rec (f : ('a -> 'b) -> 'a -> 'b) =
   let cache = Ht.create 100 in
   let rec f' n =
      try Hashtbl.find cache n
      with Not_found -> (fun fn -> Ht.add cache n fn; fn) (f f' n) in
   f'
;;

let sample_discrete a = 
   let sum = ref 0.0 in
   let len = A.length a in
   for i = 0 to len - 1 do
      sum := !sum +. A.unsafe_get a i
   done;
   let r = Random.float !sum in
   sum := 0.0;
   let i = ref (-1) in
   let fin = ref false in
   while (not !fin) do
      i := !i + 1;
      sum := !sum +. A.unsafe_get a !i;
      if r < !sum then
         fin := true
   done;
   !i
;;
let matrix_to_str f m = 
   let m' = A.map_matrix f m in
   let mx_len = A.fold_left (A.fold_left (fun a b -> max a (S.length b))) 0 m' in
   let buf = Buffer.create 5000 in
   let fmt = Scanf.format_from_string ("%" ^ (soi (mx_len+3)) ^ "s") "%s" in
   for i = 0 to A.length m' - 1 do
      let a = m'.(i) in
      for j = 0 to A.length a - 1 do
         Buffer.add_string buf (sprintf fmt a.(j))
      done;
      Buffer.add_char buf '\n'
   done;
   Buffer.contents buf
;;   

let print s = print_endline s

let brintf fmt = 
   Printf.ksprintf (fun s -> print_endline s) fmt
;;

let ebrintf fmt = 
   Printf.ksprintf (fun s -> prerr_endline s) fmt
;;
   