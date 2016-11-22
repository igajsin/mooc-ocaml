let rec print_int_list = function
  | [] -> ()
  | h::l -> print_int h; print_char '\n'; print_int_list l;;

let print_every_other k l =
  let p a b = a mod b = 0 in
  let rec wlk n = function
    | [] -> ()
    | h::tl ->
       if p n k then
         (print_int h ; print_char '\n'; wlk (n + 1) tl)
       else
         wlk (n + 1) tl in
  wlk 0 l;;

let rec print_list print = function
  | [] -> ()
  | h::l -> print h; print_char '\n'; print_list print l;;
  
