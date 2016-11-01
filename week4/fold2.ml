let for_all p l = List.fold_left (fun acc x -> acc && p x) true l;;

let exists p l = List.fold_left (fun acc x -> acc || p x) false l;;

let rec sorted cmp = function
  | [] | [_] -> true
  | h::k::r ->
    let ssorted = List.fold_left
        (fun acc x ->
           match acc with
           | (_, None) -> (x, None)
           | (prev, Some o) ->
             match cmp prev x with
             | 0 -> (x, Some o)
             | -1 when o = -1 -> (x, Some o)
             | -1 when o = 1 -> (x, None)
             | 1 when o = 1 -> (x, Some o)
             | 1 when o = -1 -> (x, None)
        ) (h, Some (cmp h k)) r in
    let (_, res) = ssorted in
    res != None;;
