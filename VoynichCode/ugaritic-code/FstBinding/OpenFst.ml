type fst 
type rng 
type state_id 
type label  = int 
type weight = float 

external test_fst_lib   : string -> unit = "testFstLib"

external new_fst        : rng -> fst * state_id = "newFst"
external add_state      : fst -> int -> state_id = "addState"
external set_final      : fst -> state_id -> weight -> unit = "setFinal"
external set_start      : fst -> state_id -> unit = "setStart"
external get_start      : fst -> state_id = "getStart"
external add_arc        : fst -> state_id -> state_id -> label -> weight -> unit = "addArc"

external new_rng        : int -> rng = "newRng"
external random         : rng -> float -> float = "rand_float"

external get_beta_      : fst -> state_id -> float = "getBeta"
external get_best_      : fst -> state_id -> float = "getBest"
external normalize      : fst -> unit = "normalize"
external sample         : fst -> int array = "sample"
external get_best_edits : fst -> int array = "getBestEdits"

external compose        : fst -> fst -> fst = "compose"
external intersect      : fst -> fst -> fst = "intersect_"
external determinize    : fst -> fst = "determinize"

external reweight_fsa   : fst -> unit = "reweightFst"

(* these next functions modify their first argument (but return it anyway for convenience*)
external union          : fst -> fst -> fst = "union_"
external concat         : fst -> fst -> fst = "concat"
external project_input  : fst -> fst = "projectInput"
external project_output : fst -> fst = "projectOutput"
external minimize       : fst -> fst = "minimize"
external sort_arcs      : fst -> fst = "arcSort"

external shortest_path  : fst -> fst = "shortestPath"
external write_to_file  : fst -> string -> bool = "writeToFile"
external read_from_file : string -> fst = "readFromFile"

external get_all_edits : fst -> int array array = "getAllEdits"

let get_beta fst = 
   let sid = get_start fst in
   if Obj.magic sid = -1 then infinity
   else get_beta_ fst sid
;;

let get_best fst = 
   let sid = get_start fst in
   if Obj.magic sid = -1 then infinity
   else get_best_ fst sid
;;

(* let rec replicate m x = match m with
    | 0 -> []
    | _ -> x :: replicate (m-1) x
let rec enumerate' m l = match l with
    | []      -> []
    | (x::xs) -> (m, x) :: enumerate' (m+1) xs
let enumerate = enumerate' 0 *)

(* let const_fst win wout =
    let (fst, start) = new_fst () in
    let statesIn  = List.map add_state (replicate (String.length win ) fst) in
    let statesOut = List.map add_state (replicate (String.length wout) fst) in
    List.iter2 (fun (n, a) b -> add_arc fst a b win.[n]  (char_of_int 0))
               (enumerate (start::statesIn)) statesIn;
    List.iter2 (fun (n, a) b -> add_arc fst a b (char_of_int 0) wout.[n])
               (enumerate (start::statesOut)) statesOut;
    fst *)