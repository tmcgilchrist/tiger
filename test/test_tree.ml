type tree = Leaf of int | Node of tree * tree

let leaf x = Leaf x
let node x y = Node (x,y)

let tree_gen = QCheck.Gen.(sized @@ fix
  (fun self n -> match n with
    | 0 -> map leaf nat
    | n ->
      frequency
        [1, map leaf nat;
         2, map2 node (self (n/2)) (self (n/2))]
               ))

let arbitrary_tree =
  let open QCheck.Iter in
  let rec shrink_tree = function
    | Leaf i -> QCheck.Shrink.int i >|= leaf
    | Node (a,b) ->
      (shrink_tree a >|= fun a' -> node a' b)
      <+>
      (shrink_tree b >|= fun b' -> node a b')
  in
  QCheck.make tree_gen ~shrink:shrink_tree

let rec mirror_tree (t:tree) : tree = match t with
  | Leaf _ -> t
  | Node (a,b) -> node (mirror_tree b) (mirror_tree a)

let tree_infix (t:tree): int list =
  let rec aux acc t = match t with
    | Leaf i -> i :: acc
    | Node (a,b) ->
      aux (aux acc b) a
  in
  aux [] t

let test_mirror =
  QCheck.Test.make ~name:"mirror_tree" ~count:200
    arbitrary_tree
    (fun (t:tree) ->
       List.rev (tree_infix t) = tree_infix (mirror_tree t))

let _ =
  let open OUnit in
  run_test_tt_main
    ("tree tests" >:::
       List.map QCheck_runner.to_ounit_test [test_mirror])
