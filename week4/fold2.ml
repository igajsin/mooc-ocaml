let for_all p l = List.fold_left (fun acc x -> acc && p x) true l;;

let exists p l = List.fold_left (fun acc x -> acc || p x) false l;;

type ordering =
  | None
  | Unordered
  | Option of int;;

let sorted cmp = function
  | [] -> true
  | h::tl ->
    let l (ord, prev) x =
      let cur_ord = cmp prev x in
      match ord with
      | None ->
        if cur_ord = 0 then
          (None, x)
        else
          (Option cur_ord, x)
      | Option o when o >= 0 && cur_ord >= 0 -> (Option 1, x)
      | Option o when o <= 0 && cur_ord <= 0 -> (Option (-1), x)
      | _ -> (Unordered, x) in
    let (res, _) = List.fold_left l (None, h) tl in
    not (res = Unordered);;
