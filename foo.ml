open Core_kernel.Std

let alist = ["a", 0; "b", 1]

module Reverse = Comparator.Make(
struct
  type t = string
  let sexp_of_t = String.sexp_of_t
  let t_of_sexp = String.t_of_sexp
  let compare x y = String.compare y x
end
)

(*
   Build a Map supplying a comparison function that gets
   stored as part of the type of the resulting Map type.
   Note the (string, int, 'a) tuple returned, where 'a is
   the comparison function type.
*)

let ord_map = Map.of_alist_exn ~comparator:String.comparator alist
(*val ord_map : (string, int, String.comparator_witness) Map.t = <abstr> *)

let rev_map = Map.of_alist_exn ~comparator:Reverse.comparator alist
(* val rev_map : (string, int, Reverse.comparator_witness) Map.t = <abstr> *)

(*
   A Tree is the same thing just without embedding the Comparison function
   within the resulting type.
*)
let ord_tree = Map.to_tree ord_map

let rev_tree = Map.to_tree rev_map
