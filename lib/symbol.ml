open Core_kernel.Std
open Sexplib.Std

type symbol = int * string [@@deriving sexp]
type t = symbol

let symbol (name : string) : symbol =
  let table = Hashtbl.create ~random:true 128 in
  let n = ref (-1) in
  try
    Hashtbl.find table name, name
  with Not_found ->
    incr n;
    Hashtbl.add table name !n;
    !n, name

let name (n : symbol) : string = snd n

module SymbolOrd = struct
  type t = symbol

  let sexp_of_t = sexp_of_symbol
  let t_of_sexp = symbol_of_sexp
  let compare = Pervasives.compare
end

module Table = Map.Make (SymbolOrd)
