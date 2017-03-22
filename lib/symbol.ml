type t = int * string

let symbol (name : string) : t=
  let table = Hashtbl.create 128 in
  let n = ref (-1) in
  try
    Hashtbl.find table name, name
  with Not_found ->
    incr n;
    Hashtbl.add table name !n;
    !n, name

let name (n : t) : string = snd n

module Ord = struct
  type symbol = t
  type t = symbol

  let compare (n1, _sym1) (n2, _sym2) =
    Pervasives.compare n1 n2
end

module Table = Map.Make (Ord)
