let filter p l = List.fold_left (fun acc x -> if p x then x::acc else acc) [] l;;

let partition p l =
  List.fold_right (fun x acc ->
      match acc with
      | (lpos, lneg) -> if p x then (x::lpos, lneg)
        else
          (lpos,x::lneg)
    ) l ([],[]);;

let rec sort = function
  | [] -> []
  | h::r -> let (smaller, higher) = partition (function x -> x < h) r in
    (sort smaller)@[h]@(sort higher);;
