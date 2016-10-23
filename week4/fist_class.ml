type int_ff = int -> int;;

let rec compose l v =
  match l with
  | [] -> v
  | x::xs -> x (compose xs v);;

let rec fixedpoint f start delta =
  let new_x = f start in
  let fabs x =
    if x >= 0. then x
    else
      (-1.) *. x in
  if fabs (start -. new_x)  < delta then
    start
  else
    fixedpoint f new_x delta;;
