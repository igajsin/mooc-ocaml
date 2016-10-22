type e = EInt of int | EMul of e * e | EAdd of e * e;;


let simplify = function
  | EMul (EInt 1, e) 
  | EMul (e, EInt 1)
  | EAdd (EInt 0, e)
  | EAdd (e, EInt 0) -> e      
  | EMul (EInt 0, e)
  | EMul (e, EInt 0) -> EInt 0
  | e -> e

let only_small_lists = function
  | x :: ([_]|[]) as l -> l
  | _ -> [];;


let rec no_consecutive_repetition = function
  | [] | [_] as l-> l
  | x :: y :: ys when x = y ->
        no_consecutive_repetition (y :: ys)
  | x :: y :: yx when x != y ->
        x :: (no_consecutive_repetition (y :: yx))

