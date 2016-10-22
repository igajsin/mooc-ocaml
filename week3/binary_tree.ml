type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

let rec height t =
  match t with
  | Empty -> 0
  | Node (left, _, right) -> 1 + max (height left) (height right);;

let rec balanced = function
  | Empty -> true
  | Node (Empty, _, Empty) -> true
  | Node (left, _, right) ->
    balanced left && balanced right &&
    height left = height right;;
