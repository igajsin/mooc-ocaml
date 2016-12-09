type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist ;;

let nil () =
  { pointer = Nil } ;;

let cons elt rest =
  { pointer = List (elt, rest) } ;;

exception Empty_xlist ;;

let head l =
  match l.pointer  with
  | Nil -> raise Empty_xlist
  | List (x,_) -> x;;

let tail l =
  match l.pointer  with
  | Nil -> raise Empty_xlist
  | List (_, tl) -> tl;;

let add a l =
  let old = nil () in
  old.pointer <- l.pointer;
  l.pointer <- List (a, old);;

let chop l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (_, rest) -> l.pointer <- rest.pointer;;

let rec append l l' =
  match l.pointer with
  | Nil -> l.pointer <- l'.pointer
  | List (h, tl) -> append tl l';;

let rec filter p l =
  match l.pointer with
  | Nil -> ()
  | List (a, l') when p a -> filter p l'
  | List (_, l') -> (l.pointer <- l'.pointer;
                     filter p l');;

let rec print_int_xlist xl =
  match xl.pointer with
  | Nil -> print_string " Nil"
  | List (a, xlst) ->
     begin
       print_string " (";
       print_int a;
       print_int_xlist xlst;
       print_string ")"
     end;;

let rec filter p l =
  match l.pointer with
  | Nil -> ()
  | List (a, l') when p a ->
     (debug_print a true l l';
     filter p l')
  | List (a, l') -> (
    debug_print a false l l';
    l.pointer <- l'.pointer;
    print_string "l:  "; print_int_xlist l; print_string "\n";
    filter p l);;
