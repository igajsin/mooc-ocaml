type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

let rec lookup_function s = function
  | [] -> invalid_arg "lookup_function"
  | (x,f)::xs -> if x = s then f
      else
        lookup_function s xs;;

let rec add_function s f = function
  | [] -> [(s,f)]
  | (x, f1)::xs ->
      if x = s then (s, f)::xs
      else
        (x,f1) :: add_function s f xs;;

let my_env = [("min", fun x y -> if x < y then x else y);
              ("add", fun x y -> x + y);
              ("sub", fun x y -> x - y);
              ("mul", fun x y -> x * y);
              ("div", fun x y -> x / y)];;

let rec compute env = function 
  | Value x -> x
  | Op (s, op1, op2) -> (lookup_function s env) (compute env op1) (compute env op2);;

let rec compute_eff env = function
  | Value x -> x
  | Op (s, op1, op2) -> (lookup_function s env) (compute env op1) (compute env op2);;
