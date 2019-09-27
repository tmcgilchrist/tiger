open Core_kernel

type symbol = int * string [@@deriving sexp]
type t = symbol [@@deriving sexp_of]

let symbol (name : string) : symbol =
  let table = String.Table.create () ~size:128 in
  let n = ref (-1) in
  match Hashtbl.find table name with
  | Some x -> x
  | None ->
    incr n;
    Hashtbl.add_exn table ~key:name ~data:(!n,name);
    !n, name

let name (n : symbol) : string = snd n

module SymbolOrd = struct
  type t = symbol

  let sexp_of_t = sexp_of_symbol
  let t_of_sexp = symbol_of_sexp
  let compare = Pervasives.compare
end

module Table = Map.Make (SymbolOrd)
