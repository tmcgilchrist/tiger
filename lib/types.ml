module Unique = struct
  type t = int
end

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * Unique.t
  | Array of t * Unique.t
  | Nil
  | Unit
  | Name of Symbol.t * t option ref
