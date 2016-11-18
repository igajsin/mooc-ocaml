type image = int -> int -> bool ;;

let all_white = fun x y -> false ;;

let all_black = fun x y -> true ;;

let checkers = fun x y -> y/2 mod 2 = x/2 mod 2 ;;

let square cx cy s = fun x y ->
  let minx = cx - s / 2 in
  let maxx = cx + s / 2 in
  let miny = cy - s / 2 in
  let maxy = cy + s / 2 in
  x >= minx && x <= maxx && y >= miny && y <= maxy
;;

let disk cx cy r = fun x y ->
  let x' = x - cx in
  let y' = y - cy in
  (x' * x' + y' * y') <= r * r
;;

type blend =
  | Image of image
  | And of blend * blend
  | Or of blend * blend
  | Rem of blend * blend
;;

let display_image width height image =
  for y = 0 to height do
    for x = 0 to width do
      if (image x y) then print_string "#"
      else print_string " "
    done;
      print_string "\n"
  done;;

let rec render blend x y =
  match blend with
  | Image f -> f x y
  | And (f, g) -> (render f x y) && (render g x y)
  | Or (f,  g) -> (render f x y) || (render g x y)
  | Rem (f, g) -> (render f x y) && not (render g x y);;

let display_blend width height blend =
  for y = 0 to height do
    for x = 0 to width do
      if (render blend x y) then print_string "#"
      else print_string " "
    done;
    print_string "\n"
  done;;
