(* Exercise 1 from Chapter 1 *)
module B = struct
  type key = string
  type tree = Leaf
            | Tree of tree * key * tree

  let empty = Leaf

  let rec insert key = function
    | Leaf -> Tree(Leaf, key, Leaf)
    | Tree(l,k,r) ->
       if key < k then Tree(insert key l, k, r)
       else if key > k then Tree(l,k, insert key r)
       else Tree(l, key, r)

  let rec member key = function
    | Leaf -> false
    | Tree(l,k,r) ->
       if key = k then true
       else if key > k then member key r
       else false
end

module KV = struct
  type key = string
  type 'a kv_tree = KVLeaf
                  | KVTree of ('a kv_tree) * key * 'a * ('a kv_tree)

  let empty = KVLeaf

  let rec kv_insert key value = function
    | KVLeaf -> KVTree(KVLeaf, key, value, KVLeaf)
    | KVTree(l,k,v,r) ->
       if key < k then KVTree(kv_insert key value l, k, v, r)
       else if key > k then KVTree(l,k, v, kv_insert key value r)
       else KVTree(l, key, value, r)

  let rec member key = function
    | KVLeaf -> false
    | KVTree(l,k,v,r) ->
       if key = k then true
       else if key > k then member key r
       else false
end

(* TODO Reseach Balanced Search Trees in Okasaki *)
