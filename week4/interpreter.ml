type operation =
    Op of string * operation * operation
  | Value of int;;

type env = (string * (int -> int -> int)) list;;

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

let rec compute e (f, op1, op2) =
    let fn = lookup_function f e in
    match (op1, op2) with
    | (Value x, Value y) -> fn x y
    | (Value x, Op (s, o1, o2)) -> fn x (compute e (s, o1, o2))
    | (Op (s, o1, o2), Value y) -> fn (compute e (s, o1, o2)) y
    | (Op (s, o1, o2), Op (s1, o3 ,o4)) -> fn (compute e (s, o1, o2)) (compute e (s1,o3,o4));;

let rec compute env = function
  | Value x -> x
  | Op (s, op1, op2) -> (lookup_function s env) (compute env op1) (compute env op2);;
