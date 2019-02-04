(*pp $PP *)

(**  
     The lattice module.  NB:  weights on edges should represent probabilities, but
     they needn't sum to one.  i.e. the structure of the lattice may have implicit 
     conditioning on some outside event.

*)
 
open Printf
open Util
(* open Sexplib
open Conv
open Graph *)

(** [(i,j)] represents the [j]th state at time [i] *) 
type node = int * int
(* edges_to{trg} is a list of (src,w) where w is the weight of the edge from src to trg *)
type 'a lattice = {nodes : 'a array array;
		   (** [nodes.(i).(j)] represents state [j] at time [i] *)
		   edges_from : (node, (node * float) array) Ht.t; 
		   edges_to : (node, (node * float) array) Ht.t;
		   edges : ((node * node), float) Ht.t; (** edges and their weights *) 
		   mutable alphas : (node,float) Ht.t  option;
		   mutable betas : (node,float) Ht.t  option;
		   mutable z : float option}
(* module Edge = struct
  type t = float
  let compare = compare
  let default = 0.0
end
module G = Imperative.Digraph.AbstractLabeled (struct type t = string end) (Edge)

module Display = struct
  include G
  let vertex_name = G.V.label
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes e = [`Label (string_of_float (G.E.label e)); `Labelfloat true]
  let get_subgraph _ = None
end
module Dot_ = Graphviz.Dot(Display)

let output_graph string_of_node lattice f = 
  let nodes, edges_from = lattice.nodes, lattice.edges_from in
  let g = G.create () in
  for i = 0 to A.length nodes - 1 do
    for j = 0 to A.length nodes.(i) - 1 do
      let src = G.V.create ((sprintf "(%d,%d) " i j) ^ (string_of_node nodes.(i).(j))) in
      A.iter 
	(fun ((i',j'),f) -> 
	   let trg = G.V.create ((sprintf "(%d,%d) " i' j') ^ (string_of_node nodes.(i').(j'))) in
	   let edge = G.E.create src f trg in
	   G.add_edge_e g edge)
	(Ht.get edges_from (i,j) [||])
    done
  done;
  let oc = open_out f in
  Dot_.output_graph oc g;
  close_out oc

let sexp_of_node = sexp_of_pair sexp_of_int sexp_of_int
let print_nodes f l = 
  let n = l.nodes in
  let s = Sexp.to_string_hum
    (sexp_of_array (sexp_of_array f) n) in
  print_endline s

let print_edges l = 
  let e = l.edges in
  let s = Sexp.to_string_hum
    (sexp_of_hashtbl (sexp_of_pair sexp_of_node sexp_of_node) sexp_of_float e) in
  print_endline s *)

(** [mk_lattice nodes edges] makes a lattice datatype. *)
let mk_lattice (nodes : 'a array array) (edges : ((node * node), float) Ht.t) : 'a lattice =
  if A.length nodes.(0) != 1 then 
    raise (Invalid_argument "nodes.(0) must have length one (the single start node)");
  if A.length nodes.(A.length nodes - 1) != 1 then
    raise (Invalid_argument "nodes(length nodes - 1) must have length one (the final node");
  
  let n = 4 * A.length nodes.(1) * A.length nodes in 
  let edges_from, edges_to = Ht.create n, Ht.create n in
  Ht.iter 
    (fun (src,trg) w -> 
      Ht.replace edges_from src (trg, w);
      Ht.replace edges_to trg (src,w))
    edges;
  let edges_from, edges_to = mht_to_ht edges_from, mht_to_ht edges_to in
  {nodes = nodes; edges = edges; edges_from = edges_from; edges_to = edges_to; 
   alphas = None; betas = None; z = None}
		    

(** [get_viterbi_path l] returns the highest probability path through lattice [l].*)
let get_viterbi_path (l : 'a lattice) to_string : node list = 
  (* output_graph to_string l "graph.dot"; *)
  let nodes, edges_to = l.nodes, l.edges_to in
  let len = A.length nodes in
  let pi, bp = Ht.create 1000, Ht.create 1000 in  (*dynamic program back-pointer tables *)
  Ht.replace pi (0,0) 1.;
  for i = 1 to (len - 1) do
    for j = 0 to A.length nodes.(i) - 1 do 
      let trg = (i,j) in
(*       print_endline  *)
(* 	(Sexp.to_string_hum  *)
(* 	   (sexp_of_hashtbl  *)
(* 	      sexp_of_node (sexp_of_array (sexp_of_pair sexp_of_node sexp_of_float)) edges_to)); *)
(*       printf "Length: edges_to = %d\n" (Ht.length edges_to); *)
(*       printf "(%d, %d)\n" i j; *)
      if not (Ht.mem edges_to trg) then () else
      let in_edges = Ht.find edges_to trg in
      let in_scores = 
	A.map 
	  (fun (src, w) -> w *. (Ht.get pi src 0.)) 
	  in_edges in
      let mx_i, mx_scr = max_itemi in_scores in
      let bst_src = fst in_edges.(mx_i) in
      Ht.replace pi trg mx_scr;
      Ht.replace bp trg bst_src
    done
  done;
  (* trace through the back-pointers... *)
(*   let s = Sexp.to_string_hum *)
(*     (sexp_of_hashtbl sexp_of_node sexp_of_node bp) in *)
(*   print_endline s; *)
  let rec f (t,n) l =
    if t = 0 then l
    else
      let hd = Ht.find bp (t,n) in
      f hd (hd :: l) in
  let final = (len - 1, 0) in
  f final [final]

(** [get_viterbi_symbols l] returns the symbols along the highest probability path
    through lattice [l]. *)
let get_viterbi_symbols (l : 'a lattice) to_string : 'a list = 
  let p = get_viterbi_path l to_string in
  L.map (fun (i,j) -> l.nodes.(i).(j)) p

(** 
    alpha.(i).(j) is alpha_j (i) in Mike Collins' notation
    i.e. alpha.(i).(j) is the sum of weights of all paths from the source to Node (i,j) 
    [i.e. state j at time i] 
*)
let get_alphas (l : 'a lattice) : (node,float) Ht.t  =
  let nodes, edges_to = l.nodes, l.edges_to in
  let len = A.length nodes in
  let alph = Ht.create 1000 in
  Ht.replace alph (0,0) 1.;
  for i = 1 to (len - 1) do 
    for j = 0 to A.length nodes.(i) - 1 do 
      let trg = (i,j) in
      Ht.replace alph trg (A.fold_left
			    (fun acc (src, w) -> acc +. w *. Ht.find alph src) 0.
			    (Ht.find edges_to trg))
    done
  done;
  alph


(** 
    beta.(i).(j) is beta_j (i) in Mike Collins' notation 
    i.e. beta.(i).(j) is the sum of weights of all paths from Node (i,j) to the target 
*)
let get_betas (l : 'a lattice) : (node,float) Ht.t =
  let nodes, edges_from = l.nodes, l.edges_from in
  let len = A.length nodes in
  let beta = Ht.create 1000 in
  Ht.replace beta (len - 1, 0) 1.;
  for i = (len - 2) downto 0 do
    for j = 0 to A.length nodes.(i) - 1 do
	let src = (i,j) in
	Ht.replace beta src (A.fold_left
			      (fun acc (trg, w) -> acc +. w *. Ht.find beta trg) 0.
			      (Ht.find edges_from src))
    done
  done;
  beta


(** 
    Get the sum of weights of all paths through the lattice (partition function).
    If the lattice represents a markov model conditioned on some fact, then this
    will give us the probability of that fact.
*)
let get_z (l : 'a lattice) : float = 
  if l.alphas = None then l.alphas <- Some (get_alphas l);
  if l.betas = None then l.betas <- Some (get_betas l);
  let acc = ref 0. 
  and a, b = Opt.get l.alphas, Opt.get l.betas in
  for i = 0 to A.length l.nodes.(0) - 1 do
    acc +.= (Ht.find a (0,i) *. Ht.find b (0,i))
  done;
  !acc
  
(** [get_node_prob l n] returns the probability of passing through node [n] in 
    lattice [l].*)
let get_node_prob (l : 'a lattice) (n : node) : float = 
  if l.z = None then l.z <- Some (get_z l);
  let a, b, z = Opt.get l.alphas, Opt.get l.betas, Opt.get l.z in 
  (Ht.find a n) *. (Ht.find b n) /. z
  
(** [get_edge_prob l src trg] returns the probability of traversing the edge from 
    node [src] to node [trg] in lattice [l]. *)
let get_edge_prob (l : 'a lattice) (src : node) (trg : node) : float =
  if not (Ht.mem l.edges (src,trg)) then 0.
  else
    let w = Ht.find l.edges (src,trg) in
    if l.z = None then l.z <- Some (get_z l);
    let a, b, z = Opt.get l.alphas, Opt.get l.betas, Opt.get l.z in
    (Ht.find a src) *. w *. (Ht.find b trg) /. z

(* (\** get the probability of path p through the lattice *\) *)
(* let get_path_prob (l : 'a lattice) (p : int array) : float =  *)
(*   if l.z = None then l.z <- Some (get_z l); *)
(*   assert (A.length l.nodes = A.length p); *)
(*   let edges, z, starts, ends = l.edges, Opt.get l.z, l.starts, l.ends in *)
(*   let prob = ref starts.( p.(0) )  in *)
(*   for i = 0 to A.length p - 2 do *)
(*     prob *.= entry_get edges ((i,p.(i)), (i,p.(i+1))) 0. *)
(*   done; *)
(*   prob *.= ends.(p.(A.length p - 1)); *)
(*   prob /.= z; *)
(*   !prob *)
      
