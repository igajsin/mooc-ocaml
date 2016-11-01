type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l = List.map (function x -> [x]) l;;

let rec tree_map f = function
  | Leaf x -> Leaf (f x)
  | Node (t1, x, t2) -> Node (tree_map f t1, f x, tree_map f t2);;
