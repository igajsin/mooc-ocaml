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
  | List (h, tl) -> if p h then
                      filter p tl
                    else
                      begin
                        l <- tl;
                        filter p tl
                      end
  | Nil -> ();;
